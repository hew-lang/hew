// Generated from the Rust sandbox package and instruction types. Run make sandbox-bytecode.
export type ActorField = { mutable: boolean; deferred: boolean };
export type ActorHandler = {
  name: string;
  message_id: number;
  every_ns?: number | null;
  callable: number;
  params: number;
  streams: boolean;
  fallible: boolean;
  result_shape: number | null;
};
export type ActorOperation =
  | { op: "identity" }
  | { op: "spawn"; actor: number }
  | { op: "self_handle"; actor: number }
  | ({ op: "stop" } & ({ actor: number } | { supervisor: number }))
  | ({ op: "terminate" } & ({ actor: number } | { supervisor: number }))
  | ({ op: "await_stopped" } & ({ actor: number } | { supervisor: number }))
  | ({ op: "await_restarted" } & ({ actor: number } | { supervisor: number }))
  | { op: "call_start"; protocol: ActorProtocol }
  | { op: "call_take"; protocol: ActorProtocol }
  | { op: "submit"; actor: number; policy: SendPolicy }
  | { op: "stream_start"; actor: number; message: number }
  | {
      op: "timer_schedule";
      actor: number;
      message: number;
      resource: number;
      policy: TimerSendPolicy;
    }
  | { op: "local_observation"; kind: string }
  | { op: "supervisor_spawn"; supervisor: number }
  | { op: "supervisor_child"; supervisor: number; child: number }
  | { op: "supervisor_pool_view"; supervisor: number; child: number };
export type ActorOverflow =
  | "block"
  | "drop_new"
  | "drop_old"
  | "fail"
  | "coalesce";
export type ActorProtocol = {
  actor: number;
  message: number;
  policy: SendPolicy;
  sealed: boolean;
};
export type ActorShape = {
  id: number;
  name: string;
  state_fields: Array<ActorField>;
  state_type?: number | null;
  init?: number | null;
  start?: number | null;
  stop: Array<number>;
  crash?: number | null;
  crash_info?: number | null;
  crash_kind?: number | null;
  crash_action?: number | null;
  exit?: number | null;
  down?: number | null;
  handlers: Array<ActorHandler>;
  mailbox_capacity?: number | null;
  overflow: ActorOverflow;
  coalesce?: Coalesce | null;
  max_heap_bytes?: string | null;
};
export type AggregateBuiltin = "Timestamp";
export type AggregateShape = {
  id: number;
  name: string;
  fields: Array<string>;
  builtin?: AggregateBuiltin | null;
};
export type ArrayValueOp = "Len" | "Index" | "IndexBorrow" | "Set";
export type AskDetail = {
  actor: number;
  message: number;
  policy: SendPolicy;
  sealed: boolean;
};
export type AsyncIoOp =
  | "TcpRead"
  | "TcpWrite"
  | "TcpAccept"
  | "TcpConnect"
  | "TcpConnectTimeout"
  | "StdinReadLine"
  | "SinkFinish"
  | "TlsConnect"
  | "TlsRead"
  | "TlsWrite"
  | "FileRead"
  | "FileWrite"
  | "WebSocketAccept"
  | "WebSocketSendText"
  | "WebSocketSendBinary";
export type BlockV1 = {
  id: number;
  params: Array<ValueDef>;
  ops: Array<OpV1>;
  term: TermV1;
};
export type BoundaryDecision =
  | "borrow"
  | "borrow_mut"
  | "copy"
  | "move"
  | "snapshot";
export type BoundaryOperand = { value: number; decision: BoundaryDecision };
export type CallResult = null | CallResultNever | ValueDef;
export type CallResultNever = "never";
export type Capability =
  | "Eq"
  | "Hash"
  | "Less"
  | "LessEqual"
  | "Greater"
  | "GreaterEqual";
export type CheckedFailure = { trap: TrapName; edge: Edge };
export type ClosureEntry = { body: number; specialization: number | null };
export type ClosureShape = {
  id: number;
  entries: Array<ClosureEntry>;
  fields: number;
};
export type Coalesce = { fallback: CoalesceFallback; keys: Array<CoalesceKey> };
export type CoalesceFallback = "drop_new" | "drop_old" | "fail";
export type CoalesceKey = { message: number; param: number };
export type CodecDirection = "encode" | "decode" | "test_input";
export type CodecKey = { name: string; tag: number | null; flags: number };
export type CodecNode =
  | "Unit"
  | "Bool"
  | { Signed: { min: number; max: number } }
  | { Unsigned: { max: number } }
  | { Float: { bits: number } }
  | "Char"
  | "String"
  | "Bytes"
  | { Tuple: Array<number> }
  | { Array: { element: number; len: number } }
  | { Sequence: number }
  | { Set: number }
  | { Map: { key: number; value: number } }
  | { Record: Array<[CodecKey, number]> }
  | { Variant: Array<[CodecKey, number | null]> }
  | { Option: number };
export type CodecShape = {
  root: number;
  nodes: Array<CodecNode>;
  names: Array<string>;
  aggregates: Array<number | null>;
  variants: Array<number | null>;
  /**
   * Synthetic tuple or record groups expand into separate enum payload fields.
   */
  grouped_payloads: Array<boolean>;
};
export type DecodedCodec = {
  shape: number;
  ok: number;
  error: number;
  error_root: number;
};
export type DirectInvocation = { direct: number };
export type DynInvocation = {
  dyn: number;
  method: string;
  receiver: BoundaryOperand;
};
export type Edge = { to: number; args: Array<number> };
export type EncodingFormat = "Json" | "Yaml" | "Toml";
export type EncodingOp =
  | "Parse"
  | "LastError"
  | "Stringify"
  | "Type"
  | "IntStatus"
  | "GetBool"
  | "GetInt"
  | "GetU64"
  | "GetFloat"
  | "GetString"
  | "GetDatetime"
  | "GetField"
  | "ArrayLen"
  | "ArrayGet"
  | "ObjectKeys"
  | "ObjectNew"
  | "ArrayNew"
  | "FromBool"
  | "FromInt"
  | "FromU64"
  | "FromFloat"
  | "FromString"
  | "FromNull"
  | "Eq"
  | "ObjectSet"
  | "ArrayPush"
  | "Clone"
  | "Free";
export type EntryExit = "unit" | "status";
export type EntryPlan = { function: number; exit: EntryExit };
export type ExternEntry = {
  id: number;
  symbol: string;
  /**
   * The capability manifest's label for a native-only runtime endpoint,
   * so a refusal can name what needs native execution. Admission still
   * keys on `symbol` alone.
   */
  native_capability?: string | null;
};
export type FileReadOp =
  | "Open"
  | "IsValid"
  | "Collect"
  | "Close"
  | "StreamOpen"
  | "StreamIsValid"
  | "StreamCollect"
  | "HasError"
  | "LastErrorKind"
  | "LastErrno"
  | "LastError";
export type FloatMethodOp =
  | "ToBits"
  | "IsNan"
  | "IsFinite"
  | "IsInfinite"
  | "IsSignNegative";
export type FunctionV1 = {
  actor_construction?: boolean;
  id: number;
  name: string;
  params: Array<ValueDef>;
  entry: number;
  places: Array<PlaceDecl>;
  blocks: Array<BlockV1>;
};
export type IndirectInvocation = {
  receiver: BoundaryOperand;
  specialization: number | null;
};
export type IntArithKind =
  | "WrappingAdd"
  | "WrappingSub"
  | "WrappingMul"
  | "SaturatingAdd"
  | "SaturatingSub"
  | "SaturatingMul"
  | "CheckedAdd"
  | "CheckedSub"
  | "CheckedMul";
export type IntBitOp =
  | "CountOnes"
  | "CountZeros"
  | "LeadingZeros"
  | "TrailingZeros"
  | "SwapBytes"
  | "ReverseBits"
  | "RotateLeft"
  | "RotateRight";
export type IntMethodWidth =
  | "I8"
  | "I16"
  | "I32"
  | "I64"
  | "Isize"
  | "U8"
  | "U16"
  | "U32"
  | "U64"
  | "Usize";
export type InvocationCallee =
  | DirectInvocation
  | MutableInvocation
  | DynInvocation
  | IndirectInvocation;
export type JoinDetail = { scope: number; mode: JoinMode };
export type JoinMode = "wait" | "cancel_losers" | "propagate_fault";
export type ManagedHandle = "actor" | "pointer" | "borrow" | "cancellation";
export type ManagedProvider =
  | "task"
  | "stream"
  | "sink"
  | "json"
  | "yaml"
  | "toml"
  | "opaque";
export type ManagedType = {
  id: number;
  /**
   * Canonical width of a slot holding this value, encoded without JS rounding.
   */
  slot_bytes: string;
} & (
  | { kind: "scalar" }
  | { kind: "handle"; category: ManagedHandle }
  | { kind: "string" }
  | { kind: "bytes" }
  | { kind: "vector"; element: number; share_on_copy: boolean }
  | { kind: "array"; element: number; length: string; share_on_copy: boolean }
  | { kind: "slice"; element: number; share_on_copy: boolean }
  | { kind: "map"; key: number; value: number }
  | { kind: "set"; element: number }
  | { kind: "aggregate"; fields: Array<number> }
  | {
      kind: "variant";
      shape: number;
      indirect: boolean;
      payload_bytes: string;
      cases: Array<Array<number>>;
    }
  | { kind: "callable"; captures: Array<number> | null }
  | {
      kind: "generator";
      yielded: number;
      returned: number;
      payload_bytes: string;
    }
  | { kind: "shared"; payload: number; weak: boolean }
  | { kind: "dynamic" }
  | {
      kind: "actor_call";
      completion: number;
      reply: number;
      declared_failure: boolean;
    }
  | {
      kind: "provider";
      provider: ManagedProvider;
      children: Array<number>;
      invocation_slot_bytes?: string;
    }
);
export type MapValueOp =
  | "New"
  | "Len"
  | "Index"
  | "Get"
  | "GetBorrow"
  | "ContainsKey"
  | "Insert"
  | "Remove"
  | "Clear"
  | "Keys"
  | "Values"
  | "Entries";
export type MathIntrinsic =
  | "Sqrt"
  | "Exp"
  | "Log"
  | "Sin"
  | "Cos"
  | "AbsI64"
  | "MinI64"
  | "MaxI64"
  | "AbsF64"
  | "MinF64"
  | "MaxF64"
  | "Pow"
  | "Floor"
  | "Ceil"
  | "Round"
  | "Tan"
  | "Asin"
  | "Acos"
  | "Atan"
  | "Atan2"
  | "Sinh"
  | "Cosh"
  | "Tanh"
  | "Exp2"
  | "Log2"
  | "Log10"
  | "Log1p"
  | "Expm1"
  | "Cbrt"
  | "Hypot"
  | "Fma"
  | "Trunc"
  | "Copysign"
  | "Powi"
  | "FromBits";
export type MutableInvocation = {
  mutable: number;
  receiver: BoundaryOperand;
  staged: boolean;
};
export type NativeIoDetail = { operation: AsyncIoOp };
export type Nonfinite = "nan" | "inf" | "-inf";
export type OffloadDetail = { function: number };
export type OpV1 = {
  definitions?: Array<ValueDef>;
  own?: Own | null;
  span?: SpanV1 | null | null;
} & (
  | { op: "coverage.hit"; counter: number }
  | { op: "const.int"; dst: number; value: string; ty: string }
  | { op: "const.bool"; dst: number; value: boolean }
  | {
      op: "const.float";
      dst: number;
      value?: number | null;
      ty: string;
      nonfinite?: Nonfinite | null;
    }
  | { op: "const.char"; dst: number; value: string }
  | { op: "const.unit"; dst: number }
  | { op: "const.duration"; dst: number; nanos: string }
  | { op: "const.str"; dst: number; str: number }
  | { op: "const.bytes"; dst: number; bytes: number }
  | { op: "copy_value"; dst: number; source: number }
  | { op: "move"; dst: number; source: number }
  | { op: "fork"; dst: number; source: number }
  | { op: "destroy_value"; dst?: number | null | null; value: number }
  | { op: "begin_borrow"; dst: number; owner: number }
  | { op: "end_borrow"; dst?: number | null | null; borrow: number }
  | { op: "finish_linear_receiver"; dst?: number | null | null }
  | { op: "alloc_place"; dst?: number | null | null; place: number }
  | {
      op: "store.init";
      dst?: number | null | null;
      place: number;
      value: number;
    }
  | {
      op: "store.assign";
      dst?: number | null | null;
      place: number;
      value: number;
    }
  | { op: "load.copy"; dst: number; place: number }
  | { op: "load.take"; dst: number; place: number }
  | { op: "load.borrow"; dst: number; place: number }
  | { op: "end_lifetime"; dst?: number | null | null; place: number }
  | { op: "tuple.make"; dst: number; elements: Array<number> }
  | { op: "tuple.get"; dst: number; tuple: number; index: number }
  | {
      op: "aggregate.make";
      dst: number;
      shape: number | null;
      fields: Array<number>;
    }
  | {
      op: "aggregate.project_copy";
      dst: number;
      shape: number | null;
      aggregate: number;
      field: number;
    }
  | {
      op: "aggregate.project_borrow";
      dst: number;
      shape: number | null;
      aggregate: number;
      field: number;
    }
  | {
      op: "destructure";
      dst?: number | null | null;
      shape: number | null;
      aggregate: number;
      results: Array<number>;
    }
  | { op: "array.make"; dst: number; elements: Array<number> }
  | { op: "array.repeat"; dst: number; value: number }
  | {
      op: "variant.make";
      dst: number;
      shape: number;
      variant: number;
      fields: Array<number>;
    }
  | {
      op: "variant.is";
      dst: number;
      shape: number;
      variant: number;
      source: number;
    }
  | {
      op: "variant.project_copy";
      dst: number;
      shape: number;
      variant: number;
      source: number;
      field: number;
    }
  | {
      op: "variant.project_borrow";
      dst: number;
      shape: number;
      variant: number;
      source: number;
      field: number;
    }
  | {
      op: "variant.destructure";
      dst?: number | null | null;
      shape: number;
      variant: number;
      source: number;
      results: Array<number>;
    }
  | { op: "unary"; dst: number; unary_op: string; value: number; ty: string }
  | {
      op: "binary";
      dst: number;
      binary_op: string;
      lhs: number;
      rhs: number;
      ty: string;
    }
  | { op: "cast"; dst: number; value: number; from: string; to: string }
  | { op: "str.eq"; dst: number; lhs: number; rhs: number }
  | { op: "bytes.eq"; dst: number; lhs: number; rhs: number }
  | { op: "function.make"; dst: number; callable: number }
  | { op: "closure.make"; dst: number; closure: number; fields: Array<number> }
  | {
      op: "callable.coerce";
      dst: number;
      source: number;
      specialization: number | null;
    }
  | { op: "dyn.make"; dst: number; vtable: number; value: number }
  | {
      op: "register_defer";
      dst?: number | null | null;
      defer: number;
      scope: number;
      dependencies: Array<number>;
    }
  | { op: "generator.make"; dst: number; closure: number; callable: number }
  | {
      op: "actor.ingress_adapter";
      dst?: number | null | null;
      actor: number;
      message: number;
    }
  | {
      op: "stream.pipe";
      dst?: number | null | null;
      capacity: number;
      results: Array<ValueDef>;
    }
  | {
      op: "task_scope.enter";
      dst?: number | null | null;
      scope: number;
      parent: number | null;
      duration: number | null;
    }
  | { op: "task_scope.close"; dst?: number | null | null; scope: number }
  | {
      op: "task.race";
      dst: number;
      scope: number;
      members: Array<number>;
      member_slot_bytes: string;
    }
  | { op: "task.spawn"; dst: number; scope: number; callable: number }
  | {
      op: "task.invoke";
      dst: number;
      scope: number;
      callee: InvocationCallee;
      args: Array<BoundaryOperand>;
    }
);
export type Own = "none" | "owned" | "guaranteed";
export type PackageV1 = {
  /**
   * Compiler-authored language accounting layouts; widths are never host sizes.
   */
  managed_types?: Array<ManagedType>;
  codecs?: Array<string>;
  structural_render?: Array<StructuralRecipe>;
  resources?: Array<Resource>;
  schema_version: SchemaVersion;
  hew_version: string;
  compiler_version: string;
  profile: string;
  entry?: EntryPlan | null;
  strings: Array<string>;
  bytes: Array<Array<number>>;
  regex_patterns: Array<string>;
  aggregates: Array<AggregateShape>;
  variants: Array<VariantShape>;
  closures: Array<ClosureShape>;
  actors?: Array<ActorShape>;
  supervisors?: Array<SupervisorShape>;
  vtables: Array<VtableShape>;
  value_capabilities: Array<ValueCapability>;
  runtime_families: Array<RuntimeFamilyEntry>;
  externs: Array<ExternEntry>;
  /**
   * Every suspension kind the instruction stream reaches, so load-time
   * admission stays a walk of the package's own manifest.
   */
  suspend_kinds: Array<Extract<TermV1, { op: "suspend" }>["kind"]>;
  functions: Array<FunctionV1>;
};
export type PlaceBase = { place: number } | { value: number };
export type PlaceDecl =
  | { origin: "local"; id: number; managed_type?: number | null }
  | { origin: "runtime"; id: number; managed_type?: number | null }
  | {
      origin: "aggregate";
      id: number;
      managed_type?: number | null;
      base: PlaceBase;
      shape?: number | null;
      field: number;
    }
  | {
      origin: "capture";
      id: number;
      managed_type?: number | null;
      environment: number;
      field: number;
    }
  | {
      origin: "actor_state";
      id: number;
      managed_type?: number | null;
      environment: number;
      field: number;
      initialized: boolean;
    };
export type PrintKind =
  | "I32"
  | "I64"
  | "U8"
  | "U32"
  | "U64"
  | "F64"
  | "Bool"
  | "Str";
export type RequestShapes = {
  failure: number | null;
  reason: number | null;
  message: number | null;
  request: number | null;
};
export type Resource =
  | { kind: "record"; shape: number; ty?: string | null; close: number }
  | { kind: "opaque"; ty?: string | null; close: number }
  | { kind: "nominal"; ty?: string | null; release: string }
  | { kind: "task" }
  | { kind: "actor_call" }
  | { kind: "actor_request" }
  | { kind: "generator" }
  | { kind: "stream" }
  | { kind: "sink" };
export type RestartPolicy = "permanent" | "transient" | "temporary";
export type RestartStrategy =
  | "one_for_one"
  | "one_for_all"
  | "rest_for_one"
  | "simple_one_for_one";
export type RuntimeCallFamily =
  | { AsyncIo: AsyncIoOp }
  | { Semaphore: SemaphoreOp }
  | { FileRead: FileReadOp }
  | { Service: ServiceOp }
  | { Tcp: TcpOp }
  | { Encoding: { format: EncodingFormat; op: EncodingOp } }
  | "ActorDemonitor"
  | "ActorLink"
  | "ActorMonitor"
  | "ActorCaller"
  | "ActorRefusalLog"
  | "ActorUnlink"
  | "AutoMutexAlloc"
  | "AutoMutexFree"
  | "AutoMutexLock"
  | "AutoMutexUnlock"
  | "DequePopFront"
  | "DequePopBack"
  | "BytesAppend"
  | "BytesClear"
  | "BytesConcat"
  | "BytesContains"
  | "BytesDecodeUtf8"
  | "BytesDecodeUtf8Lossy"
  | "BytesEndsWith"
  | "BytesGet"
  | "BytesIndex"
  | "BytesIsEmpty"
  | "BytesLen"
  | "BytesPop"
  | "BytesPush"
  | "BytesSet"
  | "BytesSlice"
  | "BytesSliceFrom"
  | "BytesStartsWith"
  | "BytesNew"
  | "CancelTokenIsRequested"
  | "CancelTokenRelease"
  | "CancelTokenRetain"
  | "ContextActorId"
  | "ContextSupervisor"
  | "ContextTraceSpan"
  | "ActorRequestRelease"
  | "ActorCallFree"
  | "ActorRequestTake"
  | "DurationAbs"
  | "DurationHours"
  | "DurationIsZero"
  | "DurationMicros"
  | "DurationMillis"
  | "DurationMins"
  | "DurationNanos"
  | "DurationSecs"
  | "DynBoxAlloc"
  | "DynBoxFree"
  | "HashMapContainsKeyLayout"
  | "HashMapClearLayout"
  | "HashMapCloneLayout"
  | "HashMapEntriesLayout"
  | "HashMapFreeLayout"
  | "HashMapGetLayout"
  | "HashMapInsertLayout"
  | "HashMapKeysLayout"
  | "HashMapLenLayout"
  | "HashMapNew"
  | "HashMapNewWithLayout"
  | "HashMapRemoveLayout"
  | "HashMapValuesLayout"
  | "HashSetContainsLayout"
  | "HashSetClearLayout"
  | "HashSetCloneLayout"
  | "HashSetFreeLayout"
  | "HashSetInsertLayout"
  | "HashSetIsEmptyLayout"
  | "HashSetLenLayout"
  | "HashSetNew"
  | "HashSetNewWithLayout"
  | "HashSetRemoveLayout"
  | "HashSetToVecLayout"
  | "InstantDurationSince"
  | "InstantElapsed"
  | "InstantNow"
  | "TimestampNow"
  | { MathIntrinsic: MathIntrinsic }
  | { IntMethod: [IntBitOp, IntMethodWidth] }
  | { IntArith: [IntArithKind, IntMethodWidth] }
  | { FloatMethod: FloatMethodOp }
  | "NodeConnect"
  | "NodeId"
  | "NodeIdDisplay"
  | "LocationNodeId"
  | "LocationIncarnation"
  | "LocationDisplay"
  | "NodeIdentityKey"
  | "NodeLoadKeys"
  | "NodeLookup"
  | "NodeRegister"
  | "NodeSetTransport"
  | "NodeShutdown"
  | "NodeStart"
  | "MetricCounterRegister"
  | "MetricCounterInc"
  | "MetricCounterAdd"
  | "MetricGaugeRegister"
  | "MetricGaugeSet"
  | "MetricGaugeInc"
  | "MetricGaugeDec"
  | "MetricGaugeAdd"
  | "MetricHistogramRegister"
  | "MetricHistogramRegisterSimple"
  | "MetricHistogramRecord"
  | "MetricVecRegister"
  | "MetricVecWith"
  | "ObserveReadU64"
  | "ObserveScrape"
  | "ObserveSeries"
  | "ObserveBarrier"
  | "RcClone"
  | "RcDowngrade"
  | "RcDrop"
  | "RcGet"
  | "RcIsUnique"
  | "RcNew"
  | "RcSet"
  | "RcStrongCount"
  | "RcWeakCount"
  | "WeakCloneRc"
  | "WeakDropRc"
  | "WeakUpgradeRc"
  | "RegexCapture"
  | "RegexCompile"
  | "RegexFreeCapture"
  | "RegexHandle"
  | "RegexMatch"
  | "ReplyChannelCancel"
  | "ReplyChannelFree"
  | "ReplyChannelNew"
  | "ReplyPayloadFree"
  | "ReplyWait"
  | "SelectFirst"
  | "SinkClone"
  | "SinkClose"
  | "StreamClose"
  | "StreamChunks"
  | "StreamForward"
  | "StreamPipe"
  | "StreamFromGenerator"
  | "SinkFromEncoder"
  | "StreamLines"
  | "StreamNextLayout"
  | "StreamPairSink"
  | "StreamPairStream"
  | "StreamSendLayout"
  | "StreamTake"
  | "StreamTryNextLayout"
  | "StreamTrySendLayout"
  | "CharIsDigit"
  | "CharIsLetter"
  | "CharIsAlphanumeric"
  | "CharIsWhitespace"
  | "CharIsUpper"
  | "CharIsLower"
  | "CharIsPunctuation"
  | "CharIsControl"
  | "CharIsAscii"
  | "CharToUpper"
  | "CharToLower"
  | "CharToTitle"
  | "CharLenUtf8"
  | "CharToDigit"
  | "CharFromI64"
  | "CharFromU64"
  | "StringGet"
  | "StringGetRange"
  | "StringSliceRange"
  | "StringAppend"
  | "StringPush"
  | "StringClear"
  | "StringFindFrom"
  | "StringByteLen"
  | "StringConcat"
  | "StringEquals"
  | "StringCompare"
  | "StringStartsWith"
  | "StringEndsWith"
  | "StringContains"
  | "StringIsEmpty"
  | "StructuralFormat"
  | "StringFind"
  | "StringIndex"
  | "StringLen"
  | "StringRepeat"
  | "StringReplace"
  | "StringClone"
  | "StringSplit"
  | "StringLines"
  | "StringSliceCodepoints"
  | "StringSliceCodepointsFrom"
  | "StringToLowercase"
  | "StringToBytes"
  | "StringToUppercase"
  | "StringTrim"
  | "U8ToString"
  | "I32ToString"
  | "I64ToString"
  | "U32ToString"
  | "U64ToString"
  | "F64ToString"
  | "CharToString"
  | { Print: { kind: PrintKind; newline: boolean } }
  | "ProcessExit"
  | "StderrWrite"
  | "BoolToString"
  | "SupervisorDirectId"
  | "SupervisorChildGet"
  | "LocalPidSupervisorChildGet"
  | "SupervisorNestedGet"
  | "SupervisorPoolChildGet"
  | "LocalPidSupervisorPoolChildRefGet"
  | "SupervisorPoolLen"
  | "SupervisorStop"
  | "TlsAttachLocal"
  | "WebSocketAttachLocal"
  | "TaskFree"
  | "GeneratorFree"
  | { Vector: VecValueOp }
  | { Array: ArrayValueOp }
  | { SupervisorPool: SupervisorPoolOp }
  | { Map: MapValueOp }
  | { Set: SetValueOp }
  | "VecAppend"
  | "VecClear"
  | "VecClone"
  | "VecCloneLayout"
  | "VecCloneOwned"
  | "VecContainsLayout"
  | "VecContainsOwned"
  | { VecContainsScalar: VecContainsScalarElem }
  | { VecGet: VecGetElem }
  | "VecIsEmpty"
  | "VecJoinStr"
  | "VecLen"
  | "VecNew"
  | "VecPopBool"
  | "VecPopLayout"
  | "VecPopOwned"
  | "VecPushBool"
  | "VecPushLayout"
  | "VecPushOwned"
  | "VecPushOwnedMove"
  | { VecScalar: { op: VecScalarOp; elem: VecScalarElem } }
  | "VecRemoveAtBool"
  | "VecRemoveAtLayout"
  | "VecRemoveAtOwned"
  | "VecSetBool"
  | "VecSetLayout"
  | "VecSetOwned"
  | "VecSetOwnedMove"
  | { VecSliceRange: VecSliceElem }
  | "VtableDispatchPanicOnOob";
export type RuntimeVariantRole =
  | "CrashKindFault"
  | "CrashKindHeapExceeded"
  | "CrashKindPartitionDetected"
  | "CrashKindHandlerFailed"
  | "CrashKindTimerOverdue"
  | "OptionSome"
  | "OptionNone"
  | "ResultOk"
  | "ResultErr"
  | "ActorErrorRejected"
  | "ActorErrorFailed"
  | "ActorErrorTrapped"
  | "ActorErrorRefused"
  | "CallerActor"
  | "CallerTask"
  | "CallerTimer"
  | "CallerNode"
  | "ActorErrorDead"
  | "ActorErrorTimedOut"
  | "ActorErrorNodeNotRunning"
  | "ActorErrorRoutingFailed"
  | "ActorErrorEncodeFailed"
  | "ActorErrorMessageTooLarge"
  | "ActorErrorConnectionDropped"
  | "ActorErrorPartition"
  | "SendErrorFull"
  | "SendErrorClosed"
  | "SendErrorPartition"
  | "SendErrorStaleRef"
  | "SendErrorLocalShutdown"
  | "SendErrorCancelled"
  | "SendErrorVersionMismatch"
  | "SendErrorBackpressure"
  | "SendErrorDead"
  | "SendErrorUnauthorized"
  | "SendErrorInvalidMessage"
  | "SendErrorMessageTooLarge"
  | "SendErrorTimedOut"
  | "SendErrorWriteTimedOut"
  | "DeliveryAccepted"
  | "DeliveryDiscarded"
  | "LinkErrorNoContext";
export type SafepointDetail = { budget: number };
export type SchemaVersion = "hew.sandbox.bytecode.v1";
export type SelectDetail = { has_timeout: boolean; order: SelectionOrder };
export type SelectionOrder = "source" | "completion";
export type SemaphoreAcquireDetail = { operation: SemaphoreOp };
export type SemaphoreOp = "Acquire" | "AcquireTimeout";
export type SendPolicy = "reject" | "wait" | "drop_newest";
export type ServiceAcquireDetail = { actor: number; export: boolean };
export type ServiceOp =
  | "Peer"
  | "Export"
  | "Receive"
  | "ClosePeer"
  | "CloseExport";
export type SetValueOp =
  | "New"
  | "Len"
  | "Contains"
  | "Insert"
  | "Remove"
  | "Clear"
  | "Elements";
export type SpanV1 = { start: number; end: number };
export type StreamNextDetail = { park: boolean };
export type StreamSendDetail = { park: boolean };
export type StructuralCase = {
  name: string;
  kind: StructuralCaseKind;
  fields: Array<StructuralField>;
};
export type StructuralCaseKind = "unit" | "tuple" | "struct";
export type StructuralField = { name: string; recipe: number };
export type StructuralRecipe =
  | { kind: "scalar" }
  | { kind: "unit" }
  | { kind: "display"; callee: number }
  | { kind: "identity"; name: string }
  | { kind: "tuple"; members: Array<number> }
  | { kind: "vector"; members: Array<number> }
  | { kind: "map"; members: Array<number> }
  | { kind: "record"; name: string; fields: Array<StructuralField> }
  | { kind: "enum"; cases: Array<StructuralCase> };
export type SubmissionShapes = {
  success: number | null;
  failure: number | null;
  reason: number | null;
};
export type SupervisedRole = { actor: number } | { supervisor: number };
export type SupervisorChild = {
  name: string;
  dependencies?: Array<number>;
  /**
   * The role this child plays, as `{"actor": id}` or `{"supervisor": id}`.
   */
  role: SupervisedRole;
  restart: RestartPolicy;
  stop_ns?: number | null;
  stop_config?: number | null;
  is_pool?: boolean;
  spawn: number;
};
export type SupervisorPoolOp = "Len" | "Member" | "Get";
export type SupervisorShape = {
  id: number;
  strategy: RestartStrategy;
  max_restarts: number;
  window_secs: number;
  backoff?: boolean;
  children: Array<SupervisorChild>;
  startup_order?: Array<number>;
};
export type SwitchArm = {
  variant: number;
  fields: Array<ValueDef>;
  edge: Edge;
};
export type TcpOp =
  | "Listen"
  | "ListenerValid"
  | "ConnectionValid"
  | "ListenerPort"
  | "ListenerClose"
  | "ConnectionClose"
  | "ReadTimeout"
  | "WriteTimeout"
  | "BroadcastExcept";
export type TermV1 = { span?: SpanV1 | null | null } & (
  | { op: "materialize"; operation: OpV1; normal: Edge; failure: Edge }
  | { op: "return"; value: BoundaryOperand | null }
  | { op: "goto"; edge: Edge }
  | { op: "branch"; condition: number; then: Edge; else: Edge }
  | {
      op: "switch.variant";
      shape: number;
      scrutinee: number;
      arms: Array<SwitchArm>;
    }
  | {
      op: "checked.binary";
      binary_op: string;
      lhs: number;
      rhs: number;
      result: ValueDef;
      normal: Edge;
      failures: Array<CheckedFailure>;
      ty: string;
    }
  | {
      op: "timestamp.binary";
      binary_op: TimestampBinaryOp;
      lhs: number;
      rhs: number;
      lhs_timestamp: boolean;
      rhs_timestamp: boolean;
      shape: number | null;
      result: ValueDef;
      normal: Edge;
      failures: Array<CheckedFailure>;
    }
  | { op: "unreachable" }
  | {
      op: "call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      callee: number;
      handback?: ValueDef | null | null;
    }
  | {
      op: "indirect.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      callee: BoundaryOperand;
      specialization: number | null;
    }
  | {
      op: "dyn.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      receiver: BoundaryOperand;
      slot: number;
    }
  | {
      op: "value.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      plan: number;
    }
  | {
      op: "wire.codec";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      codec: number;
      format: number;
      direction: CodecDirection;
      decoded: DecodedCodec | null;
      callbacks: { [key in string]: Array<number> };
    }
  | {
      op: "runtime.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      shared_payload_type?: string | null;
      family: number;
      structural?: number | null | null;
      callbacks?: Array<number> | null;
      releases_contents?: boolean | null;
      keeps_inputs_on_failure?: boolean | null;
      result_member_shapes?: Array<number | null> | null;
      variant_result_shapes?: VariantResultShapes | null;
      result_shape: number | null;
      /**
       * Checked aggregate descriptor supplied by the SIR result type.
       */
      aggregate_result_shape?: number | null | null;
    }
  | {
      op: "extern.call";
      /**
       * Exact resource release slot selected from the checked result type.
       */
      result_resource?: number | null;
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      extern: number;
      result_shape: number | null;
      /**
       * Checked aggregate descriptor supplied by the SIR result type.
       */
      aggregate_result_shape?: number | null | null;
    }
  | {
      op: "actor.call";
      args: Array<BoundaryOperand>;
      result: CallResult;
      normal?: Edge | null | null;
      unwind: Edge | null;
      request_shapes?: RequestShapes | null | null;
      operation: ActorOperation;
      submission_shapes?: SubmissionShapes | null | null;
      result_shape: number | null;
      error_shape: number | null;
    }
  | {
      op: "panic";
      message: BoundaryOperand;
      assertion?:
        | [BoundaryOperand, BoundaryOperand, BoundaryOperand]
        | null
        | null;
      cleanup: Edge;
    }
  | { op: "trap"; trap: TrapName }
  | { op: "checked_raise"; trap: TrapName; cleanup: Edge }
  | { op: "cleanup.dispatch"; normal: Edge; fault: Edge }
  | { op: "resume_unwind"; handback?: BoundaryOperand | null | null }
  | { op: "enter_defer"; defer: number; park: number; body: Edge }
  | { op: "finish_defer"; defer: number; park: number; next: Edge }
  | {
      op: "recover_fault";
      result_shape?: number | null | null;
      result: ValueDef;
      deadline_variant: number;
      fault_variant: number;
      normal: Edge;
      unwind: Edge | null;
    }
  | ({
      op: "suspend";
      request_shapes?: RequestShapes | null | null;
      result_shape?: number | null | null;
      error_shape?: number | null | null;
      inputs: Array<BoundaryOperand>;
      result: CallResult;
      resumes: Array<Edge>;
      cancel: Edge | null;
      unwind: Edge | null;
    } & (
      | { kind: "Sleep"; detail?: null | null | null }
      | { kind: "SleepUntil"; detail?: null | null | null }
      | { kind: "Await"; detail?: null | null | null }
      | { kind: "ActorSend"; detail?: null | null | null }
      | { kind: "RestartWait"; detail?: null | null | null }
      | { kind: "Read"; detail?: null | null | null }
      | { kind: "Accept"; detail?: null | null | null }
      | { kind: "CallClosure"; detail?: null | null | null }
      | { kind: "Timeout"; detail?: null | null | null }
      | { kind: "ScopeDeadline"; detail?: null | null | null }
      | { kind: "Yield"; detail?: null | null | null }
      | { kind: "GeneratorNext"; detail?: null | null | null }
      | { kind: "Safepoint"; detail: SafepointDetail }
      | { kind: "ServiceAcquire"; detail: ServiceAcquireDetail }
      | { kind: "SemaphoreAcquire"; detail: SemaphoreAcquireDetail }
      | { kind: "NativeIo"; detail: NativeIoDetail }
      | { kind: "Offload"; detail: OffloadDetail }
      | { kind: "Ask"; detail: AskDetail }
      | { kind: "Join"; detail: JoinDetail }
      | { kind: "StreamSend"; detail: StreamSendDetail }
      | { kind: "StreamNext"; detail: StreamNextDetail }
      | { kind: "Select"; detail: SelectDetail }
    ))
);
export type TimerSendPolicy = "reject" | "drop_newest";
export type TimestampBinaryOp = "Add" | "Subtract";
export type TrapName =
  | "heap_exceeded"
  | "integer_overflow"
  | "divide_by_zero"
  | "shift_out_of_range"
  | "vector_bounds"
  | "actor_refused";
export type ValueCapability = {
  id: number;
  capability: Capability;
  ty: string;
  callable?: number | null;
  components?: Array<ValueComponentCapabilities>;
  variants?: Array<Array<ValueComponentCapabilities>> | null;
};
export type ValueComponentCapabilities = {
  selected: number;
  ordering?: ValueOrderingCapabilities | null;
};
export type ValueDef = {
  value: number;
  own: Own;
  managed_type?: number | null;
};
export type ValueOrderingCapabilities = {
  less: number;
  less_equal: number;
  greater: number;
  greater_equal: number;
};
export type VariantCase = { name: string; fields: Array<string> };
export type VariantResultShapes = { error: number; error_len: number };
export type VariantShape = {
  id: number;
  name: string;
  cases: Array<VariantCase>;
  runtime_tags?: { [key in RuntimeVariantRole]?: number };
};
export type VecContainsScalarElem = "F64" | "I32" | "I64" | "Str";
export type VecGetElem =
  | "Bool"
  | "F32"
  | "F64"
  | "I8"
  | "I16"
  | "I32"
  | "I64"
  | "Clone"
  | "Take"
  | "Layout"
  | "Owned"
  | "Ptr"
  | "Str"
  | "U8"
  | "U16";
export type VecScalarElem =
  | "F32"
  | "F64"
  | "I8"
  | "I16"
  | "I32"
  | "I64"
  | "Ptr"
  | "Str"
  | "U8"
  | "U16";
export type VecScalarOp = "Push" | "Pop" | "Set" | "RemoveAt";
export type VecSliceElem =
  | "Bytesize"
  | "F64"
  | "I32"
  | "I64"
  | "Layout"
  | "Owned"
  | "Ptr"
  | "Str";
export type VecValueOp =
  | "Swap"
  | "New"
  | "Len"
  | "Contains"
  | "Index"
  | "Get"
  | "Push"
  | "Set"
  | "Pop"
  | "Remove"
  | "Clear"
  | "IndexBorrow"
  | "GetBorrow"
  | "TakeFirst"
  | "Slice"
  | "SliceFrom"
  | "Append"
  | "Join";
export type VtableShape = { id: number; slots: Array<VtableSlot> };
export type VtableSlot = {
  slot: number;
  method: string;
  callee: number;
  /**
   * How the erased receiver crosses the dispatch boundary. A `dyn.call`
   * passes the wrapped value to the callee's `self` parameter under this
   * decision, which the verifier proves matches the boundary the concrete
   * type was erased under.
   */
  receiver: BoundaryDecision;
  receiver_handback?: boolean;
};
type RuntimeIdentity<T> = T extends string
  ? { family: T; detail?: null }
  : { [K in keyof T]: { family: K; detail: T[K] } }[keyof T];
export type RuntimeFamilyEntry = {
  id: number;
} & RuntimeIdentity<RuntimeCallFamily>;
export const OP_NAMES = [
  "coverage.hit",
  "const.int",
  "const.bool",
  "const.float",
  "const.char",
  "const.unit",
  "const.duration",
  "const.str",
  "const.bytes",
  "copy_value",
  "move",
  "fork",
  "destroy_value",
  "begin_borrow",
  "end_borrow",
  "finish_linear_receiver",
  "alloc_place",
  "store.init",
  "store.assign",
  "load.copy",
  "load.take",
  "load.borrow",
  "end_lifetime",
  "tuple.make",
  "tuple.get",
  "aggregate.make",
  "aggregate.project_copy",
  "aggregate.project_borrow",
  "destructure",
  "array.make",
  "array.repeat",
  "variant.make",
  "variant.is",
  "variant.project_copy",
  "variant.project_borrow",
  "variant.destructure",
  "unary",
  "binary",
  "cast",
  "str.eq",
  "bytes.eq",
  "function.make",
  "closure.make",
  "callable.coerce",
  "dyn.make",
  "register_defer",
  "generator.make",
  "actor.ingress_adapter",
  "stream.pipe",
  "task_scope.enter",
  "task_scope.close",
  "task.race",
  "task.spawn",
  "task.invoke",
] as const;
export const TERM_NAMES = [
  "materialize",
  "return",
  "goto",
  "branch",
  "switch.variant",
  "checked.binary",
  "timestamp.binary",
  "unreachable",
  "call",
  "indirect.call",
  "dyn.call",
  "value.call",
  "wire.codec",
  "runtime.call",
  "extern.call",
  "actor.call",
  "panic",
  "trap",
  "checked_raise",
  "cleanup.dispatch",
  "resume_unwind",
  "enter_defer",
  "finish_defer",
  "recover_fault",
  "suspend",
] as const;
export const SUSPENSION_NAMES = [
  "Sleep",
  "SleepUntil",
  "Await",
  "ActorSend",
  "RestartWait",
  "Read",
  "Accept",
  "CallClosure",
  "Timeout",
  "ScopeDeadline",
  "Yield",
  "GeneratorNext",
  "Safepoint",
  "ServiceAcquire",
  "SemaphoreAcquire",
  "NativeIo",
  "Offload",
  "Ask",
  "Join",
  "StreamSend",
  "StreamNext",
  "Select",
] as const;
export type OpName = OpV1["op"];
export type TermName = TermV1["op"];
