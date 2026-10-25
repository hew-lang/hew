/// Public v1 wire types derive from the Rust package contract.
/// Compiler-emitted packages project verified SIR; arbitrary imported packages
/// acquire only structural/reference checks, not an ownership proof.
import type {
  TermV1,
  BlockV1,
  OpV1,
  VariantShape,
  RuntimeVariantRole,
} from "./instructions.generated.js";
export type {
  Own,
  BoundaryDecision,
  TrapName,
  SpanV1,
  BoundaryOperand,
  Edge,
  ValueDef,
  CallResult,
  OpV1,
  OpName,
  TermV1,
  TermName,
  SwitchArm,
  CheckedFailure,
  RequestShapes,
  ActorProtocol,
  ActorOperation,
  RuntimeFamilyEntry,
  AggregateShape,
  VariantCase,
  VariantShape,
  RuntimeVariantRole,
  ExternEntry,
  PlaceDecl,
  ValueCapability,
  ValueComponentCapabilities,
  ClosureShape,
  VtableShape,
  FunctionV1,
  BlockV1,
  EntryPlan,
  ActorShape,
  SupervisorShape,
  PackageV1,
  Resource,
  StructuralRecipe,
  ManagedType,
} from "./instructions.generated.js";
export type Operand = number;
export type CallShape = Pick<
  Extract<TermV1, { op: "call" }>,
  "args" | "result" | "normal" | "unwind" | "request_shapes"
>;

export function runtimeTag(
  shape: VariantShape,
  role: RuntimeVariantRole,
): number {
  const tag = shape.runtime_tags?.[role];
  if (
    tag === undefined ||
    !Number.isInteger(tag) ||
    tag < 0 ||
    tag >= shape.cases.length
  )
    throw new Error(`variant shape ${shape.id} has no checked ${role} tag`);
  return tag;
}

/** Static inventory includes a checked operation; execution follows its edge. */
export function* staticOperations(block: BlockV1): Iterable<OpV1> {
  yield* block.ops;
  if (block.term.op === "materialize") yield block.term.operation;
}
