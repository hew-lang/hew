/// Validate references in a structurally decoded package before host admission.
/// This is a bounded graph walk, not SIR's type, dominance or ownership verifier.
import type {
  ActorOperation,
  BoundaryOperand,
  Edge,
  FunctionV1,
  OpV1,
  PackageV1,
  RequestShapes,
  TermV1,
  ValueDef,
} from "./package.js";
import type { CodecShape, CodecNode } from "./instructions.generated.js";
import { validCodecShape } from "./package-validator.generated.js";
import { staticOperations } from "./package.js";

function require(condition: unknown, message: string): asserts condition {
  if (!condition) throw new Error(message);
}

function indexed<T extends { id: number }>(
  table: readonly T[],
  name: string,
): void {
  // The executor indexes these tables directly; accepting conflicting IDs
  // would let the same reference denote different entries at different sites.
  for (const [index, entry] of table.entries())
    require(entry.id ===
      index, `${name} ID ${entry.id} does not match index ${index}`);
}

function at<T>(table: readonly T[], id: number, name: string): T {
  const value = table[id];
  require(value !== undefined, `${name} reference ${id} is absent`);
  return value;
}

function exhaustive(value: never): never {
  throw new Error(`unhandled bytecode reference carrier ${String(value)}`);
}

export function validatePackageGraph(pkg: PackageV1): void {
  const actors = pkg.actors ?? [];
  const supervisors = pkg.supervisors ?? [];
  const resources = pkg.resources ?? [];
  const recipes = pkg.structural_render ?? [];
  const managed = pkg.managed_types ?? [];
  indexed(managed, "managed type");
  const managedType = (id: number | null | undefined) =>
    id == null ? null : at(managed, id, "managed type");
  const width = (value: string) => {
    require(/^(0|[1-9][0-9]*)$/.test(
      value,
    ), "managed width is not decimal u64");
    require(BigInt(value) <= 0xffffffffffffffffn, "managed width exceeds u64");
  };
  for (const row of managed) {
    width(row.slot_bytes);
    switch (row.kind) {
      case "scalar":
      case "handle":
      case "string":
      case "bytes":
      case "dynamic":
        break;
      case "array":
        width(row.length);
        managedType(row.element);
        break;
      case "vector":
      case "slice":
      case "set":
        managedType(row.element);
        break;
      case "map":
        managedType(row.key);
        managedType(row.value);
        break;
      case "aggregate":
        row.fields.forEach(managedType);
        break;
      case "variant":
        width(row.payload_bytes);
        at(pkg.variants, row.shape, "managed variant shape");
        row.cases.forEach((fields) => fields.forEach(managedType));
        break;
      case "callable":
        row.captures?.forEach(managedType);
        break;
      case "provider":
        if (row.invocation_slot_bytes != null) width(row.invocation_slot_bytes);
        row.children.forEach(managedType);
        break;
      case "actor_call":
        managedType(row.completion);
        managedType(row.reply);
        break;
      case "generator":
        width(row.payload_bytes);
        managedType(row.yielded);
        managedType(row.returned);
        break;
      case "shared":
        managedType(row.payload);
        break;
      default:
        exhaustive(row);
    }
  }
  indexed(pkg.functions, "function");
  indexed(pkg.aggregates, "aggregate");
  indexed(pkg.variants, "variant");
  indexed(pkg.closures, "closure");
  indexed(pkg.vtables, "vtable");
  indexed(pkg.value_capabilities, "value capability");
  indexed(pkg.runtime_families, "runtime family");
  indexed(pkg.externs, "extern");
  indexed(actors, "actor");
  indexed(supervisors, "supervisor");
  const callable = (id: number) => at(pkg.functions, id, "function");
  const aggregate = (id: number | null | undefined) =>
    id == null ? null : at(pkg.aggregates, id, "aggregate");
  const variant = (id: number | null | undefined) =>
    id == null ? null : at(pkg.variants, id, "variant");
  const capability = (id: number) =>
    at(pkg.value_capabilities, id, "value capability");
  const recipe = (id: number) => at(recipes, id, "structural recipe");
  const resource = (id: number) => at(resources, id, "resource");
  const messages = actors.map((actor) => {
    const handlers = new Map(
      actor.handlers.map((handler) => [handler.message_id, handler]),
    );
    require(handlers.size ===
      actor.handlers.length, `actor ${actor.id} repeats a message`);
    return handlers;
  });
  const message = (actor: number, id: number) => {
    const handler = at(messages, actor, "actor").get(id);
    require(handler, `actor ${actor} message ${id} is absent`);
    return handler;
  };
  const requestShapes = (shapes: RequestShapes | null | undefined) => {
    if (!shapes) return;
    aggregate(shapes.failure);
    variant(shapes.reason);
    aggregate(shapes.message);
    aggregate(shapes.request);
  };
  if (pkg.entry) {
    const entry = callable(pkg.entry.function);
    require(entry.params.length === 0, "entry function requires arguments");
  }
  for (const shape of pkg.variants)
    for (const tag of Object.values(shape.runtime_tags ?? {}))
      require(tag !== undefined &&
        tag < shape.cases.length, `variant ${shape.id} runtime tag is absent`);
  for (const closure of pkg.closures) {
    const specializations = new Set<number | null>();
    for (const entry of closure.entries) {
      callable(entry.body);
      require(!specializations.has(
        entry.specialization,
      ), `closure ${closure.id} repeats a specialization`);
      specializations.add(entry.specialization);
    }
  }
  for (const table of pkg.vtables) {
    const slots = new Set<number>();
    for (const slot of table.slots) {
      require(!slots.has(
        slot.slot,
      ), `vtable ${table.id} repeats slot ${slot.slot}`);
      slots.add(slot.slot);
      callable(slot.callee);
    }
  }
  for (const plan of pkg.value_capabilities) {
    if (plan.callable != null) callable(plan.callable);
    for (const member of [
      ...(plan.components ?? []),
      ...(plan.variants ?? []).flat(),
    ]) {
      capability(member.selected);
      if (member.ordering)
        for (const selected of Object.values(member.ordering))
          capability(selected);
    }
  }
  for (const value of recipes) {
    switch (value.kind) {
      case "scalar":
      case "unit":
      case "identity":
        break;
      case "display":
        callable(value.callee);
        break;
      case "vector":
        require(value.members.length ===
          1, "vector rendering requires one member");
        value.members.forEach(recipe);
        break;
      case "map":
        require(value.members.length ===
          2, "map rendering requires two members");
        value.members.forEach(recipe);
        break;
      case "tuple":
        value.members.forEach(recipe);
        break;
      case "record":
        value.fields.forEach((field) => recipe(field.recipe));
        break;
      case "enum":
        value.cases.forEach((item) =>
          item.fields.forEach((field) => recipe(field.recipe)),
        );
        break;
      default:
        exhaustive(value);
    }
  }
  for (const value of resources) {
    switch (value.kind) {
      case "record":
        aggregate(value.shape);
        callable(value.close);
        break;
      case "opaque":
        callable(value.close);
        break;
      case "nominal":
      case "task":
      case "actor_call":
      case "actor_request":
      case "generator":
      case "stream":
      case "sink":
        break;
      default:
        exhaustive(value);
    }
  }
  for (const actor of actors) {
    if (actor.max_heap_bytes != null) width(actor.max_heap_bytes);
    managedType(actor.state_type);
    for (const id of [
      actor.init,
      actor.start,
      actor.crash,
      actor.exit,
      actor.down,
      ...actor.stop,
    ])
      if (id != null) callable(id);
    aggregate(actor.crash_info);
    variant(actor.crash_kind);
    variant(actor.crash_action);
    for (const handler of actor.handlers) {
      callable(handler.callable);
      variant(handler.result_shape);
    }
    for (const key of actor.coalesce?.keys ?? []) {
      const handler = message(actor.id, key.message);
      require(key.param <
        handler.params, "coalescing key is outside the message parameters");
    }
  }
  for (const supervisor of supervisors)
    for (const child of supervisor.children) {
      callable(child.spawn);
      if ("actor" in child.role) at(actors, child.role.actor, "actor");
      else at(supervisors, child.role.supervisor, "supervisor");
    }

  // The codec's inner JSON remains the v1 string wire format, but its shape
  // vocabulary is generated from the same Rust carrier used by native codecs.
  const codecs = (pkg.codecs ?? []).map((encoded, index) => {
    const decoded: unknown = JSON.parse(encoded);
    if (!validCodecShape(decoded)) {
      const reasons = (validCodecShape.errors ?? []).map(
        (issue: { instancePath: string; message?: string }) =>
          `${issue.instancePath || "/"} ${issue.message ?? "invalid value"}`,
      );
      throw new Error(`codec ${index}: ${reasons.join("; ")}`);
    }
    const shape = decoded as CodecShape;
    const node = (id: number) => at(shape.nodes, id, "codec node");
    node(shape.root);
    require([
      shape.names,
      shape.aggregates,
      shape.variants,
      shape.grouped_payloads,
    ].every(
      (table) => table.length === shape.nodes.length,
    ), "codec descriptor tables have different lengths");
    shape.aggregates.forEach(aggregate);
    shape.variants.forEach(variant);
    for (const value of shape.nodes) validateNode(value);
    return shape;

    function validateNode(value: CodecNode): void {
      if (typeof value === "string") return;
      if ("Tuple" in value) value.Tuple.forEach(node);
      else if ("Array" in value) node(value.Array.element);
      else if ("Sequence" in value) node(value.Sequence);
      else if ("Set" in value) node(value.Set);
      else if ("Map" in value) {
        node(value.Map.key);
        node(value.Map.value);
      } else if ("Record" in value) value.Record.forEach(([, id]) => node(id));
      else if ("Variant" in value)
        value.Variant.forEach(([, id]) => {
          if (id != null) node(id);
        });
      else if ("Option" in value) node(value.Option);
      else if ("Float" in value)
        require(value.Float.bits === 32 ||
          value.Float.bits === 64, "codec float width is invalid");
      else if ("Signed" in value)
        require(value.Signed.min <=
          value.Signed.max, "codec signed range is invalid");
      else if ("Unsigned" in value)
        require(value.Unsigned.max >= 0, "codec unsigned range is invalid");
      else exhaustive(value);
    }
  });

  function actorOperation(operation: ActorOperation): void {
    switch (operation.op) {
      case "identity":
      case "local_observation":
        break;
      case "spawn":
      case "self_handle":
      case "submit":
        at(actors, operation.actor, "actor");
        break;
      case "stop":
      case "terminate":
      case "await_stopped":
      case "await_restarted":
        if ("actor" in operation) at(actors, operation.actor, "actor");
        else at(supervisors, operation.supervisor, "supervisor");
        break;
      case "call_start":
      case "call_take":
        message(operation.protocol.actor, operation.protocol.message);
        break;
      case "stream_start":
        message(operation.actor, operation.message);
        break;
      case "timer_schedule":
        message(operation.actor, operation.message);
        resource(operation.resource);
        break;
      case "supervisor_spawn":
        at(supervisors, operation.supervisor, "supervisor");
        break;
      case "supervisor_child":
      case "supervisor_pool_view":
        at(
          at(supervisors, operation.supervisor, "supervisor").children,
          operation.child,
          "supervisor child",
        );
        break;
      default:
        exhaustive(operation);
    }
  }

  const suspensions = new Set(pkg.suspend_kinds);
  require(suspensions.size ===
    pkg.suspend_kinds.length, "suspension manifest repeats a kind");
  for (const fn of pkg.functions) validateFunction(fn);

  function validateFunction(fn: FunctionV1): void {
    indexed(fn.blocks, `function ${fn.id} block`);
    indexed(fn.places, `function ${fn.id} place`);
    const entry = at(fn.blocks, fn.entry, "entry block");
    require(entry.params.length ===
      0, "entry block has unbound block arguments");
    const values = new Set<number>();
    const scopes = new Set<number>();
    const defers = new Set<number>();
    const define = (id: number) => {
      require(!values.has(id), `function ${fn.id} repeats value ${id}`);
      values.add(id);
    };
    const value = (id: number) =>
      require(values.has(id), `function ${fn.id} value ${id} is absent`);
    const boundary = (operand: BoundaryOperand) => value(operand.value);
    const place = (id: number) => at(fn.places, id, "place");
    const scope = (id: number | null) => {
      if (id != null) require(scopes.has(id), `task scope ${id} is absent`);
    };
    const defer = (id: number) =>
      require(defers.has(id), `defer ${id} is absent`);
    const edge = (edge: Edge | null | undefined) => {
      if (!edge) return;
      const target = at(fn.blocks, edge.to, "block");
      require(target.params.length ===
        edge.args
          .length, `edge to block ${edge.to} has ${edge.args.length} arguments, expected ${target.params.length}`);
      edge.args.forEach(value);
    };
    const result = (def: ValueDef) => {
      managedType(def.managed_type);
      define(def.value);
    };
    fn.params.forEach(result);
    // Results exist on authored successor edges. This pass checks identity,
    // not dominance or whether a particular path owns the referenced value.
    for (const block of fn.blocks) {
      block.params.forEach(result);
      for (const op of staticOperations(block)) {
        op.definitions?.forEach((def) => managedType(def.managed_type));
        if ("dst" in op && op.dst != null) define(op.dst);
        if (op.op === "destructure" || op.op === "variant.destructure")
          op.results.forEach(define);
        if (op.op === "stream.pipe") op.results.forEach(result);
        if (op.op === "task_scope.enter") {
          scopes.add(op.scope);
        }
        if (op.op === "register_defer") {
          defers.add(op.defer);
        }
      }
      const term = block.term;
      if ("result" in term && term.result && term.result !== "never")
        result(term.result);
      if (term.op === "call" && term.handback) result(term.handback);
      if (term.op === "switch.variant")
        term.arms.forEach((arm) => arm.fields.forEach(result));
    }
    for (const declaration of fn.places) {
      managedType(declaration.managed_type);
      switch (declaration.origin) {
        case "local":
        case "runtime":
          break;
        case "aggregate": {
          if ("place" in declaration.base) place(declaration.base.place);
          else value(declaration.base.value);
          const shape = aggregate(declaration.shape);
          if (shape)
            require(declaration.field <
              shape.fields.length, "place field is outside its aggregate");
          break;
        }
        case "capture":
        case "actor_state":
          value(declaration.environment);
          break;
        default:
          exhaustive(declaration);
      }
    }
    // Every projected place has at most one parent. Colour each chain once;
    // cycles otherwise recurse forever in the executor's place resolver.
    const complete = new Set<number>();
    for (const declaration of fn.places) {
      const chain = new Set<number>();
      let current = declaration;
      while (!complete.has(current.id)) {
        require(!chain.has(current.id), "place projection contains a cycle");
        chain.add(current.id);
        if (current.origin !== "aggregate" || !("place" in current.base)) break;
        current = place(current.base.place);
      }
      chain.forEach((id) => complete.add(id));
    }
    for (const block of fn.blocks) {
      block.ops.forEach(operation);
      terminator(block.term);
    }

    function operation(op: OpV1): void {
      switch (op.op) {
        case "coverage.hit":
        case "const.int":
        case "const.bool":
        case "const.float":
        case "const.char":
        case "const.unit":
        case "const.duration":
        case "finish_linear_receiver":
          break;
        case "const.str":
          at(pkg.strings, op.str, "string");
          break;
        case "const.bytes":
          at(pkg.bytes, op.bytes, "bytes");
          break;
        case "copy_value":
        case "move":
        case "fork":
        case "callable.coerce":
          value(op.source);
          break;
        case "destroy_value":
        case "unary":
        case "cast":
          value(op.value);
          break;
        case "begin_borrow":
          value(op.owner);
          break;
        case "end_borrow":
          value(op.borrow);
          break;
        case "alloc_place":
        case "load.copy":
        case "load.take":
        case "load.borrow":
        case "end_lifetime":
          place(op.place);
          break;
        case "store.init":
        case "store.assign":
          place(op.place);
          value(op.value);
          break;
        case "tuple.make":
        case "array.make":
          op.elements.forEach(value);
          break;
        case "tuple.get":
          value(op.tuple);
          break;
        case "aggregate.make": {
          const shape = aggregate(op.shape);
          if (shape)
            require(shape.fields.length ===
              op.fields
                .length, "aggregate constructor has the wrong field count");
          op.fields.forEach(value);
          break;
        }
        case "aggregate.project_copy":
        case "aggregate.project_borrow": {
          const shape = aggregate(op.shape);
          if (shape)
            require(op.field <
              shape.fields.length, "aggregate projection field is absent");
          value(op.aggregate);
          break;
        }
        case "destructure": {
          const shape = aggregate(op.shape);
          if (shape)
            require(shape.fields.length ===
              op.results
                .length, "aggregate destructuring has the wrong field count");
          value(op.aggregate);
          break;
        }
        case "array.repeat":
          value(op.value);
          break;
        case "variant.make":
        case "variant.is":
        case "variant.project_copy":
        case "variant.project_borrow":
        case "variant.destructure": {
          const selected = at(
            variant(op.shape)!.cases,
            op.variant,
            "variant case",
          );
          if (op.op === "variant.make") {
            require(selected.fields.length ===
              op.fields
                .length, "variant constructor has the wrong field count");
            op.fields.forEach(value);
          } else {
            value(op.source);
            if ("field" in op)
              require(op.field <
                selected.fields.length, "variant projection field is absent");
            if ("results" in op)
              require(op.results.length ===
                selected.fields
                  .length, "variant destructuring has the wrong field count");
          }
          break;
        }
        case "binary":
        case "str.eq":
        case "bytes.eq":
          value(op.lhs);
          value(op.rhs);
          break;
        case "function.make":
          callable(op.callable);
          break;
        case "closure.make": {
          const closure = at(pkg.closures, op.closure, "closure");
          require(closure.fields ===
            op.fields
              .length, "closure constructor has the wrong capture count");
          op.fields.forEach(value);
          break;
        }
        case "dyn.make":
          at(pkg.vtables, op.vtable, "vtable");
          value(op.value);
          break;
        case "register_defer":
          op.dependencies.forEach(place);
          break;
        case "generator.make":
          at(pkg.closures, op.closure, "closure");
          value(op.callable);
          break;
        case "actor.ingress_adapter":
          message(op.actor, op.message);
          break;
        case "stream.pipe":
          require(op.results.length ===
            2, "stream pipe requires sink and stream results");
          break;
        case "task_scope.enter":
          scope(op.parent);
          if (op.duration != null) value(op.duration);
          break;
        case "task_scope.close":
          scope(op.scope);
          break;
        case "task.race":
          scope(op.scope);
          width(op.member_slot_bytes);
          op.members.forEach(value);
          break;
        case "task.spawn":
          scope(op.scope);
          value(op.callable);
          break;
        case "task.invoke": {
          scope(op.scope);
          op.args.forEach(boundary);
          if ("direct" in op.callee) callable(op.callee.direct);
          else {
            boundary(op.callee.receiver);
            if ("mutable" in op.callee) callable(op.callee.mutable);
          }
          break;
        }
        default:
          exhaustive(op);
      }
    }

    function terminator(term: TermV1): void {
      // These are common wire fields, not inferred source semantics.
      if ("args" in term) term.args.forEach(boundary);
      if ("normal" in term) edge(term.normal);
      if ("unwind" in term) edge(term.unwind);
      if ("request_shapes" in term) requestShapes(term.request_shapes);
      switch (term.op) {
        case "materialize":
          operation(term.operation);
          edge(term.failure);
          break;
        case "return":
          if (term.value) boundary(term.value);
          break;
        case "goto":
          edge(term.edge);
          break;
        case "branch":
          value(term.condition);
          edge(term.then);
          edge(term.else);
          break;
        case "switch.variant": {
          const shape = variant(term.shape)!;
          value(term.scrutinee);
          const tags = new Set<number>();
          for (const arm of term.arms) {
            const selected = at(shape.cases, arm.variant, "variant case");
            require(!tags.has(arm.variant), "variant switch repeats a case");
            tags.add(arm.variant);
            require(selected.fields.length ===
              arm.fields.length, "variant switch has the wrong payload count");
            edge(arm.edge);
          }
          require(tags.size ===
            shape.cases.length, "variant switch omits a case");
          break;
        }
        case "checked.binary":
        case "timestamp.binary":
          value(term.lhs);
          value(term.rhs);
          term.failures.forEach((failure) => edge(failure.edge));
          if (term.op === "timestamp.binary") aggregate(term.shape);
          break;
        case "call":
          require(callable(term.callee).params.length ===
            term.args.length, "direct call has the wrong argument count");
          break;
        case "indirect.call":
          boundary(term.callee);
          break;
        case "dyn.call":
          boundary(term.receiver);
          break;
        case "value.call":
          capability(term.plan);
          break;
        case "wire.codec": {
          const codec = at(codecs, term.codec, "codec");
          for (const [node, callbacks] of Object.entries(term.callbacks)) {
            require(/^(0|[1-9][0-9]*)$/.test(
              node,
            ), "codec callback node is invalid");
            at(codec.nodes, Number(node), "codec callback node");
            callbacks.forEach(capability);
          }
          if (term.decoded) {
            const shape = variant(term.decoded.shape)!;
            at(shape.cases, term.decoded.ok, "decoded success case");
            at(shape.cases, term.decoded.error, "decoded failure case");
            at(codec.nodes, term.decoded.error_root, "codec error node");
          }
          break;
        }
        case "runtime.call":
          at(pkg.runtime_families, term.family, "runtime family");
          variant(term.result_shape);
          aggregate(term.aggregate_result_shape);
          if (term.structural != null) recipe(term.structural);
          term.callbacks?.forEach(capability);
          term.result_member_shapes?.forEach(variant);
          if (term.variant_result_shapes) {
            aggregate(term.variant_result_shapes.error);
            variant(term.variant_result_shapes.error_len);
          }
          break;
        case "extern.call":
          if (term.result_resource != null)
            at(pkg.resources ?? [], term.result_resource, "resource");
          at(pkg.externs, term.extern, "extern");
          variant(term.result_shape);
          aggregate(term.aggregate_result_shape);
          break;
        case "actor.call":
          actorOperation(term.operation);
          variant(term.result_shape);
          variant(term.error_shape);
          if (term.submission_shapes) {
            variant(term.submission_shapes.success);
            aggregate(term.submission_shapes.failure);
            variant(term.submission_shapes.reason);
          }
          break;
        case "panic":
          boundary(term.message);
          term.assertion?.forEach(boundary);
          edge(term.cleanup);
          break;
        case "checked_raise":
          edge(term.cleanup);
          break;
        case "cleanup.dispatch":
          edge(term.fault);
          break;
        case "resume_unwind":
          if (term.handback) boundary(term.handback);
          break;
        case "enter_defer":
          defer(term.defer);
          edge(term.body);
          break;
        case "finish_defer":
          defer(term.defer);
          edge(term.next);
          break;
        case "recover_fault": {
          const shape = variant(term.result_shape);
          require(shape, "fault recovery lacks its result shape");
          at(shape.cases, term.deadline_variant, "deadline variant");
          at(shape.cases, term.fault_variant, "fault variant");
          break;
        }
        case "suspend":
          require(suspensions.has(
            term.kind,
          ), `suspension ${term.kind} is missing from its manifest`);
          variant(term.result_shape);
          variant(term.error_shape);
          term.inputs.forEach(boundary);
          term.resumes.forEach(edge);
          edge(term.cancel);
          switch (term.kind) {
            case "Offload":
              callable(term.detail.function);
              break;
            case "ServiceAcquire":
              at(actors, term.detail.actor, "actor");
              break;
            case "Ask":
              message(term.detail.actor, term.detail.message);
              break;
            case "Join":
              scope(term.detail.scope);
              break;
            default:
              break;
          }
          break;
        case "trap":
        case "unreachable":
          break;
        default:
          exhaustive(term);
      }
    }
  }
}
