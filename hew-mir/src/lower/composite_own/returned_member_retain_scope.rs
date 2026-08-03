use super::{
    derive_returned_aggregate_member_bindings, BasicBlock, BindingId, BuiltinType, FieldOffset,
    HashMap, HashSet, Instr, Place, ResolvedTy, StringRetainCondition, Terminator,
};

fn returned_block(construct: Instr, condition: StringRetainCondition) -> BasicBlock {
    BasicBlock {
        id: 0,
        statements: Vec::new(),
        instructions: vec![
            Instr::StringRetain {
                value: Place::Local(0),
                condition,
            },
            construct,
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        terminator: Terminator::Return,
    }
}

fn returned_members(block: BasicBlock, ty: ResolvedTy) -> HashSet<BindingId> {
    let binding = BindingId(1);
    derive_returned_aggregate_member_bindings(
        &[block],
        &[(binding, "member".to_string(), ty)],
        &HashMap::from([(binding, Place::Local(0))]),
    )
}

#[test]
fn aggregate_string_leaf_retain_does_not_clone_tuple_or_record_handle() {
    let sink_ty = ResolvedTy::named_builtin("Sink", BuiltinType::Sink, vec![ResolvedTy::String]);
    let tuple_members = returned_members(
        returned_block(
            Instr::TupleConstruct {
                elements: vec![Place::Local(0)],
                dest: Place::Local(1),
            },
            StringRetainCondition::AggregateBorrowedIngress,
        ),
        sink_ty.clone(),
    );
    assert!(
        tuple_members.contains(&BindingId(1)),
        "a string-leaf retain marker must not hide an affine tuple member \
         from return ownership transfer"
    );

    let record_members = returned_members(
        returned_block(
            Instr::RecordInit {
                ty: ResolvedTy::named_user("Pipe", vec![]),
                fields: vec![(FieldOffset(0), Place::Local(0))],
                dest: Place::Local(1),
            },
            StringRetainCondition::AggregateBorrowedIngress,
        ),
        sink_ty,
    );
    assert!(
        record_members.contains(&BindingId(1)),
        "a string-leaf retain marker must not hide an affine record field \
         from return ownership transfer"
    );
}

#[test]
fn whole_string_retain_keeps_the_callee_owner() {
    let members = returned_members(
        returned_block(
            Instr::TupleConstruct {
                elements: vec![Place::Local(0)],
                dest: Place::Local(1),
            },
            StringRetainCondition::Always,
        ),
        ResolvedTy::String,
    );
    assert!(
        !members.contains(&BindingId(1)),
        "an unconditional string retain mints a caller owner, so the callee \
         must keep and drop its original owner"
    );
}

#[test]
fn whole_bytes_retain_keeps_the_callee_owner() {
    let member = BindingId(1);
    let block = BasicBlock {
        id: 0,
        statements: Vec::new(),
        instructions: vec![
            Instr::BytesRetain {
                value: Place::Local(0),
            },
            Instr::RecordInit {
                ty: ResolvedTy::named_user("Pair", vec![]),
                fields: vec![(FieldOffset(0), Place::Local(0))],
                dest: Place::Local(1),
            },
            Instr::Move {
                dest: Place::ReturnSlot,
                src: Place::Local(1),
            },
        ],
        terminator: Terminator::Return,
    };
    let members = derive_returned_aggregate_member_bindings(
        &[block],
        &[(member, "member".to_string(), ResolvedTy::Bytes)],
        &HashMap::from([(member, Place::Local(0))]),
    );
    assert!(
        !members.contains(&member),
        "a bytes retain mints the returned record owner, so the callee must \
         keep and drop its original owner"
    );
}
