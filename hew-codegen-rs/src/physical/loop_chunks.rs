//! Realize physical scalar loop chunks outside the coroutine's spill frame.

use super::*;
use hew_mir::physical::PhysicalLoopChunk;

impl<'a, 'ctx> FunctionEmitter<'a, 'ctx> {
    pub(super) fn emit_loop_chunk(&self, chunk: &PhysicalLoopChunk) -> CodegenResult<()> {
        // Only carriers crossing the call boundary need persistent coroutine
        // storage. Private chunk temporaries remain in the helper's locals.
        let mut boundary = chunk.inputs.clone();
        for outputs in chunk.exits.values() {
            boundary.extend(outputs);
        }
        let ids = boundary.into_iter().collect::<Vec<_>>();
        let mut fields = ids
            .iter()
            .map(|id| llvm_type(self.ctx, &self.function.storage[id.0 as usize].layout.repr))
            .collect::<CodegenResult<Vec<_>>>()?;
        fields.push(self.ctx.i32_type().into());
        let record = self.ctx.struct_type(&fields, false);
        let snapshot = coro::entry_storage(
            self.ctx,
            self.value.get_first_basic_block().expect("function entry"),
            record.into(),
            "loop.snapshot",
        )?;
        let helper = self.build_loop_chunk(chunk, &ids, record)?;
        for (index, id) in ids
            .iter()
            .enumerate()
            .filter(|(_, id)| chunk.inputs.contains(id))
        {
            let address = self
                .builder
                .build_struct_gep(record, snapshot, record_index(index)?, "loop.input")
                .llvm_ctx("address loop input")?;
            self.builder
                .build_store(address, self.load(*id, "loop.input.value")?)
                .llvm_ctx("snapshot loop input")?;
        }
        let entry = self.builder.get_insert_block().expect("loop header");
        let invoke = self.ctx.append_basic_block(self.value, "loop.chunk");
        let poll = self.ctx.append_basic_block(self.value, "loop.poll");
        let after_poll = self.ctx.append_basic_block(self.value, "loop.after.poll");
        self.builder
            .build_unconditional_branch(invoke)
            .llvm_ctx("enter loop chunk")?;
        self.builder.position_at_end(after_poll);
        self.builder
            .build_unconditional_branch(invoke)
            .llvm_ctx("continue polled loop chunk")?;
        self.builder.position_at_end(invoke);
        let skip = self
            .builder
            .build_phi(self.ctx.bool_type(), "loop.polled")
            .llvm_ctx("merge loop scheduling phase")?;
        skip.add_incoming(&[
            (&self.ctx.bool_type().const_zero(), entry),
            (&self.ctx.bool_type().const_int(1, false), after_poll),
        ]);
        let frame = self
            .frame
            .as_ref()
            .expect("loop chunk has a coroutine frame");
        let enabled = self
            .builder
            .build_is_not_null(frame.budget, "loop.budget.enabled")
            .llvm_ctx("test loop poll suppression")?;
        let read_budget = self.ctx.append_basic_block(self.value, "loop.read.budget");
        let run = self.ctx.append_basic_block(self.value, "loop.run");
        self.builder
            .build_conditional_branch(enabled, read_budget, run)
            .llvm_ctx("select loop budget")?;
        self.builder.position_at_end(read_budget);
        let remaining = self
            .builder
            .build_load(self.ctx.i32_type(), frame.budget, "loop.remaining")
            .llvm_ctx("read shared loop budget")?;
        self.builder
            .build_unconditional_branch(run)
            .llvm_ctx("enter bounded loop chunk")?;
        self.builder.position_at_end(run);
        let budget = self
            .builder
            .build_phi(self.ctx.i32_type(), "loop.budget")
            .llvm_ctx("merge loop budget")?;
        budget.add_incoming(&[
            (&self.ctx.i32_type().const_zero(), invoke),
            (&remaining, read_budget),
        ]);
        let exit = suspend::call_value(
            &self.builder,
            helper,
            &[
                snapshot.into(),
                budget.as_basic_value().into(),
                skip.as_basic_value().into(),
                enabled.into(),
                self.active_fault.into(),
                self.active_status.into(),
            ],
            "loop.exit",
        )?
        .into_int_value();
        let flush = self.ctx.append_basic_block(self.value, "loop.flush.budget");
        let dispatch = self.ctx.append_basic_block(self.value, "loop.dispatch");
        self.builder
            .build_conditional_branch(enabled, flush, dispatch)
            .llvm_ctx("select loop budget synchronization")?;
        self.builder.position_at_end(flush);
        let address = self
            .builder
            .build_struct_gep(
                record,
                snapshot,
                record_index(ids.len())?,
                "loop.budget.out",
            )
            .llvm_ctx("address remaining loop budget")?;
        let remaining = self
            .builder
            .build_load(self.ctx.i32_type(), address, "loop.remaining.out")
            .llvm_ctx("load remaining loop budget")?;
        self.builder
            .build_store(frame.budget, remaining)
            .llvm_ctx("synchronize shared loop budget")?;
        self.builder
            .build_unconditional_branch(dispatch)
            .llvm_ctx("dispatch synchronized loop exit")?;
        self.builder.position_at_end(dispatch);
        // Only carriers initialized on every chunk exit are restored here.
        self.restore_loop_slots(chunk.inputs.iter().copied(), &ids, record, snapshot)?;
        let mut cases = Vec::new();
        for (index, (target, outputs)) in chunk.exits.iter().enumerate() {
            let block = self.ctx.append_basic_block(self.value, "loop.exit.restore");
            cases.push((
                self.ctx.i32_type().const_int(index as u64 + 1, false),
                block,
            ));
            self.builder.position_at_end(block);
            self.restore_loop_slots(outputs.iter().copied(), &ids, record, snapshot)?;
            self.builder
                .build_unconditional_branch(self.blocks[target])
                .llvm_ctx("leave physical loop chunk")?;
        }
        self.builder.position_at_end(dispatch);
        self.builder
            .build_switch(exit, poll, &cases)
            .llvm_ctx("select physical loop exit")?;
        self.builder.position_at_end(poll);
        let header = self
            .function
            .blocks
            .iter()
            .find(|block| block.id == chunk.header)
            .expect("chunk header");
        let PhysicalTerminator::Safepoint { cancel, .. } = &header.terminator else {
            unreachable!()
        };
        self.emit_safepoint_poll(after_poll, cancel.as_ref())
    }

    fn restore_loop_slots(
        &self,
        slots: impl IntoIterator<Item = StorageId>,
        ids: &[StorageId],
        record: inkwell::types::StructType<'ctx>,
        snapshot: PointerValue<'ctx>,
    ) -> CodegenResult<()> {
        for id in slots {
            let index = ids.binary_search(&id).expect("chunk carrier");
            let address = self
                .builder
                .build_struct_gep(record, snapshot, record_index(index)?, "loop.output")
                .llvm_ctx("address loop output")?;
            let value = self
                .builder
                .build_load(
                    llvm_type(self.ctx, &self.storage(id)?.layout.repr)?,
                    address,
                    "loop.output.value",
                )
                .llvm_ctx("restore loop output")?;
            self.restore_loop_carrier(id, value)?;
        }
        Ok(())
    }

    fn build_loop_chunk(
        &self,
        chunk: &PhysicalLoopChunk,
        ids: &[StorageId],
        record: inkwell::types::StructType<'ctx>,
    ) -> CodegenResult<FunctionValue<'ctx>> {
        let pointer = self.ctx.ptr_type(AddressSpace::default());
        let value = self.llvm.add_function(
            &format!(
                "{}.loop.{}",
                self.value.get_name().to_string_lossy(),
                chunk.header.0
            ),
            self.ctx.i32_type().fn_type(
                &[
                    pointer.into(),
                    self.ctx.i32_type().into(),
                    self.ctx.bool_type().into(),
                    self.ctx.bool_type().into(),
                    pointer.into(),
                    pointer.into(),
                ],
                false,
            ),
            Some(Linkage::Private),
        );
        // Inlining into a resumable body would make CoroSplit spill the loop's
        // PHIs again. This boundary runs once per remaining shared budget.
        value.add_attribute(
            inkwell::attributes::AttributeLoc::Function,
            self.ctx.create_enum_attribute(
                inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
                0,
            ),
        );
        let entry = self.ctx.append_basic_block(value, "entry");
        let builder = self.ctx.create_builder();
        builder.position_at_end(entry);
        let snapshot = value
            .get_nth_param(0)
            .expect("snapshot")
            .into_pointer_value();
        let mut slots = vec![pointer.const_null(); self.slots.len()];
        let mut place_flags = BTreeMap::new();
        for id in &chunk.storage {
            slots[id.0 as usize] = builder
                .build_alloca(
                    llvm_type(self.ctx, &self.storage(*id)?.layout.repr)?,
                    &format!("s{}", id.0),
                )
                .llvm_ctx("allocate chunk scalar")?;
            if self.function.place_storage.contains_key(id) {
                let flag = builder
                    .build_alloca(self.ctx.bool_type(), "loop.initialized")
                    .llvm_ctx("allocate chunk initialization flag")?;
                builder
                    .build_store(flag, self.ctx.bool_type().const_zero())
                    .llvm_ctx("initialize chunk scalar flag")?;
                place_flags.insert(*id, flag);
            }
        }
        let budget = builder
            .build_alloca(self.ctx.i32_type(), "remaining")
            .llvm_ctx("allocate local loop countdown")?;
        builder
            .build_store(budget, value.get_nth_param(1).expect("budget"))
            .llvm_ctx("initialize local loop countdown")?;
        let enabled = value
            .get_nth_param(3)
            .expect("poll enabled")
            .into_int_value();
        // Inline checked reads publish into the caller's authored fault pair.
        // The helper returns to that caller for semantic owner cleanup.
        let active_fault = value
            .get_nth_param(4)
            .expect("caller fault slot")
            .into_pointer_value();
        let active_status = value
            .get_nth_param(5)
            .expect("caller fault status")
            .into_pointer_value();
        let mut blocks = chunk
            .blocks
            .iter()
            .map(|id| {
                (
                    *id,
                    self.ctx.append_basic_block(value, &format!("bb{}", id.0)),
                )
            })
            .collect::<BTreeMap<_, _>>();
        for target in chunk.exits.keys() {
            blocks.insert(*target, self.ctx.append_basic_block(value, "exit"));
        }
        let poll = self.ctx.append_basic_block(value, "poll");
        let mut emitter = FunctionEmitter {
            module: self.module,
            target_data: self.target_data,
            function: self.function,
            ctx: self.ctx,
            llvm: self.llvm,
            builder,
            value,
            blocks,
            slots,
            place_flags,
            result_out: None,
            fault_out: active_fault,
            active_fault,
            active_status,
            fault_site: std::cell::Cell::new(None),
            fault_parks: BTreeMap::new(),
            functions: self.functions,
            value_callbacks: self.value_callbacks,
            ramps: self.ramps,
            frame: None,
            task_scopes: BTreeMap::new(),
            debug: None,
        };
        emitter.restore_loop_slots(chunk.inputs.iter().copied(), ids, record, snapshot)?;
        for constant in &chunk.constants {
            emitter.emit_op(constant)?;
        }
        let header = self
            .function
            .blocks
            .iter()
            .find(|block| block.id == chunk.header)
            .expect("header");
        let PhysicalTerminator::Safepoint { normal, .. } = &header.terminator else {
            unreachable!()
        };
        // Peel one physical iteration before the cyclic region. Checked
        // arithmetic stays on that first path, while LLVM can retain bounds
        // established by it on the register recurrence (for example remainder
        // bounds). All peeled exits still use the same snapshot protocol.
        let original_blocks = emitter.blocks.clone();
        for id in chunk.blocks.iter().filter(|id| **id != chunk.header) {
            emitter
                .blocks
                .insert(*id, self.ctx.append_basic_block(value, "loop.first"));
        }
        let after_poll = self.ctx.append_basic_block(value, "after.poll");
        let initial_tick = self.ctx.append_basic_block(value, "initial.tick");
        emitter
            .builder
            .build_conditional_branch(
                value.get_nth_param(2).expect("phase").into_int_value(),
                after_poll,
                initial_tick,
            )
            .llvm_ctx("select chunk scheduling phase")?;
        emitter.builder.position_at_end(after_poll);
        emitter.emit_edge(normal)?;
        emitter.builder.position_at_end(initial_tick);
        emitter.emit_chunk_tick(budget, enabled, poll, normal)?;
        for block in self
            .function
            .blocks
            .iter()
            .filter(|block| chunk.blocks.contains(&block.id) && block.id != chunk.header)
        {
            emitter.builder.position_at_end(emitter.blocks[&block.id]);
            for op in &block.ops {
                emitter.emit_op(op)?;
            }
            emitter.emit_terminator(block)?;
        }
        emitter.blocks = original_blocks;
        for block in self
            .function
            .blocks
            .iter()
            .filter(|block| chunk.blocks.contains(&block.id))
        {
            emitter.builder.position_at_end(emitter.blocks[&block.id]);
            if block.id == chunk.header {
                emitter.emit_chunk_tick(budget, enabled, poll, normal)?;
            } else {
                for op in &block.ops {
                    emitter.emit_op(op)?;
                }
                emitter.emit_terminator(block)?;
            }
        }
        emitter.builder.position_at_end(poll);
        emitter.finish_loop_chunk(
            chunk.inputs.iter().copied(),
            ids,
            record,
            snapshot,
            budget,
            0,
        )?;
        for (index, (target, outputs)) in chunk.exits.iter().enumerate() {
            emitter.builder.position_at_end(emitter.blocks[target]);
            emitter.finish_loop_chunk(
                chunk.inputs.union(outputs).copied(),
                ids,
                record,
                snapshot,
                budget,
                index as u64 + 1,
            )?;
        }
        Ok(value)
    }

    fn emit_chunk_tick(
        &self,
        budget: PointerValue<'ctx>,
        enabled: IntValue<'ctx>,
        poll: BasicBlock<'ctx>,
        normal: &PhysicalEdge,
    ) -> CodegenResult<()> {
        let decrement = self.ctx.append_basic_block(self.value, "countdown");
        let continuing = self.ctx.append_basic_block(self.value, "continue");
        self.builder
            .build_conditional_branch(enabled, decrement, continuing)
            .llvm_ctx("select chunk countdown")?;
        self.builder.position_at_end(decrement);
        let remaining = self
            .builder
            .build_load(self.ctx.i32_type(), budget, "remaining")
            .llvm_ctx("read local countdown")?
            .into_int_value();
        let next = self
            .builder
            .build_int_sub(remaining, self.ctx.i32_type().const_int(1, false), "next")
            .llvm_ctx("consume scheduling tick")?;
        self.builder
            .build_store(budget, next)
            .llvm_ctx("save local countdown")?;
        let exhausted = self
            .builder
            .build_int_compare(
                IntPredicate::EQ,
                next,
                self.ctx.i32_type().const_zero(),
                "exhausted",
            )
            .llvm_ctx("test local countdown")?;
        self.builder
            .build_conditional_branch(exhausted, poll, continuing)
            .llvm_ctx("select chunk budget exit")?;
        self.builder.position_at_end(continuing);
        self.emit_edge(normal)?;
        Ok(())
    }

    fn finish_loop_chunk(
        &self,
        outputs: impl IntoIterator<Item = StorageId>,
        ids: &[StorageId],
        record: inkwell::types::StructType<'ctx>,
        snapshot: PointerValue<'ctx>,
        budget: PointerValue<'ctx>,
        exit: u64,
    ) -> CodegenResult<()> {
        for id in outputs {
            let address = self
                .builder
                .build_struct_gep(
                    record,
                    snapshot,
                    record_index(ids.binary_search(&id).expect("output carrier"))?,
                    "output",
                )
                .llvm_ctx("address chunk output")?;
            self.builder
                .build_store(address, self.load(id, "output.value")?)
                .llvm_ctx("save chunk output")?;
        }
        let address = self
            .builder
            .build_struct_gep(record, snapshot, record_index(ids.len())?, "budget.out")
            .llvm_ctx("address chunk countdown output")?;
        self.builder
            .build_store(
                address,
                self.builder
                    .build_load(self.ctx.i32_type(), budget, "remaining.out")
                    .llvm_ctx("read final countdown")?,
            )
            .llvm_ctx("save chunk countdown output")?;
        self.builder
            .build_return(Some(&self.ctx.i32_type().const_int(exit, false)))
            .llvm_ctx("return physical chunk exit")?;
        Ok(())
    }
}

fn record_index(index: usize) -> CodegenResult<u32> {
    u32::try_from(index)
        .map_err(|_| CodegenError::FailClosed("loop snapshot index exceeds u32".into()))
}
