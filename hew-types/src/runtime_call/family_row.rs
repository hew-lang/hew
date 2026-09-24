//! The declarative operation-row table for every `RuntimeCallFamily` variant.

#![allow(
    clippy::wildcard_imports,
    reason = "sibling split of one module; shares its item set"
)]

use super::descriptors::*;
use super::family::RuntimeCallFamily;
use super::value_kinds::*;
use super::{declared, ArrayValueOp, FileReadOp, SupervisorPoolOp};
use crate::BuiltinType;

impl RuntimeCallFamily {
    /// The one row describing this runtime operation.
    #[must_use]
    #[expect(
        clippy::too_many_lines,
        clippy::match_same_arms,
        reason = "one declarative row per runtime operation is the authority, and \
                  two operations that happen to agree today still state their own facts"
    )]
    pub const fn row(self) -> RuntimeOpRow {
        use RuntimeArgumentContract as A;
        use RuntimeArgumentEffect as E;
        use RuntimeResultEffect as R;
        use RuntimeValueKind as K;
        match self {
            // The async IO, file and TCP operation sets and the encoding
            // matrix each carry their row in their own operation enum.
            Self::AsyncIo(op) => RuntimeOpRow {
                symbol: op.c_symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: RuntimeCReturn::Storage,
                physical: RuntimePhysicalForm::NotAnAction,
            },
            Self::FileRead(op) => RuntimeOpRow {
                symbol: op.c_symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: match op {
                    FileReadOp::IsValid | FileReadOp::StreamIsValid => RuntimeCReturn::TruthI32,
                    FileReadOp::HasError => RuntimeCReturn::TruthBool,
                    _ => RuntimeCReturn::Storage,
                },
                physical: RuntimePhysicalForm::Direct,
            },
            Self::Tcp(op) => RuntimeOpRow {
                symbol: op.c_symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: RuntimeCReturn::Storage,
                physical: RuntimePhysicalForm::Direct,
            },
            Self::Encoding { format, op } => RuntimeOpRow {
                symbol: op.c_symbol(format),
                contract: Some(op.contract(format)),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                c_return: RuntimeCReturn::Storage,
                physical: RuntimePhysicalForm::Direct,
            },
            Self::SupervisorPool(op) => RuntimeOpRow {
                symbol: op.symbol(),
                contract: Some(op.contract()),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                // Only `get` builds an `Option<ChildRef<T>>` descriptor; the
                // trapping and awaiting forms hand back the member itself.
                physical: match op {
                    SupervisorPoolOp::Get => RuntimePhysicalForm::VariantResult,
                    SupervisorPoolOp::Member | SupervisorPoolOp::AwaitRestartMember => {
                        RuntimePhysicalForm::Direct
                    }
                },
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorAsk => RuntimeOpRow {
                symbol: "hew_actor_ask",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorAskWithChannel => RuntimeOpRow {
                symbol: "hew_actor_ask_with_channel",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorCooperate => RuntimeOpRow {
                symbol: "hew_actor_cooperate",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorDemonitor => RuntimeOpRow {
                symbol: "hew_actor_demonitor",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorGenSinkComplete => RuntimeOpRow {
                symbol: "hew_actor_gen_sink_complete",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorGenSinkRegister => RuntimeOpRow {
                symbol: "hew_actor_gen_sink_register",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorLink => RuntimeOpRow {
                symbol: "hew_actor_link",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LinkRemote => RuntimeOpRow {
                symbol: "hew_node_link_remote_location",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorMonitor => RuntimeOpRow {
                symbol: "hew_actor_monitor",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorSelf => RuntimeOpRow {
                symbol: "hew_actor_self",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorSendById => RuntimeOpRow {
                symbol: "hew_actor_send_by_id",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorSpawn => RuntimeOpRow {
                symbol: "hew_actor_spawn",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorUnlink => RuntimeOpRow {
                symbol: "hew_actor_unlink",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexAlloc => RuntimeOpRow {
                symbol: "hew_auto_mutex_alloc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexFree => RuntimeOpRow {
                symbol: "hew_auto_mutex_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexLock => RuntimeOpRow {
                symbol: "hew_auto_mutex_lock",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::AutoMutexUnlock => RuntimeOpRow {
                symbol: "hew_auto_mutex_unlock",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesAppend => RuntimeOpRow {
                symbol: "hew_bytes_append",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Move,
                        },
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesClear => RuntimeOpRow {
                symbol: "hew_bytes_clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesContains => declared::BYTESCONTAINS.row,
            Self::BytesDecodeUtf8 => RuntimeOpRow {
                symbol: "hew_bytes_decode_utf8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwnedVariant(RuntimeVariantResultKind::Utf8Decode),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Utf8Decode,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesDecodeUtf8Lossy => RuntimeOpRow {
                symbol: "hew_bytes_decode_utf8_lossy",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesGet => RuntimeOpRow {
                symbol: "hew_bytes_get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesIndex => RuntimeOpRow {
                symbol: "hew_bytes_index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesIsEmpty => declared::BYTESISEMPTY.row,
            Self::BytesLen => declared::BYTESLEN.row,
            Self::BytesPop => RuntimeOpRow {
                symbol: "hew_bytes_pop",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bytes,
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Bytes,
                        K::Applied(BuiltinType::Option, &[K::U8]),
                    ])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::PairWithOption,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesPush => RuntimeOpRow {
                symbol: "hew_bytes_push",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Move,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesSet => RuntimeOpRow {
                symbol: "hew_bytes_set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Bytes),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesSlice => RuntimeOpRow {
                symbol: "hew_bytes_slice",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesSliceFrom => RuntimeOpRow {
                symbol: "hew_bytes_slice_from",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Bytes,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BytesNew => RuntimeOpRow {
                symbol: "bytes::new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::BytesConstructor,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CancelTokenIsRequested => RuntimeOpRow {
                symbol: "hew_cancel_token_is_requested",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CancelTokenRelease => RuntimeOpRow {
                symbol: "hew_cancel_token_release",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CancelTokenRetain => RuntimeOpRow {
                symbol: "hew_cancel_token_retain",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorRequestRelease => RuntimeOpRow {
                symbol: "hew_msg_envelope_release",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ActorRequestOwner,
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorCallFree => RuntimeOpRow {
                symbol: "hew_actor_call_free",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::ActorCall),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ActorRequestTake => RuntimeOpRow {
                symbol: "hew_actor_ask_wait_take_request",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::ActorRequestAdmission,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::ActorRequestOwner),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DurationAbs => declared::DURATIONABS.row,
            Self::DurationHours => declared::DURATIONHOURS.row,
            Self::DurationIsZero => declared::DURATIONISZERO.row,
            Self::DurationMicros => declared::DURATIONMICROS.row,
            Self::DurationMillis => declared::DURATIONMILLIS.row,
            Self::DurationMins => declared::DURATIONMINS.row,
            Self::DurationNanos => declared::DURATIONNANOS.row,
            Self::DurationSecs => declared::DURATIONSECS.row,
            Self::DynBoxAlloc => RuntimeOpRow {
                symbol: "hew_dyn_box_alloc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::DynBoxFree => RuntimeOpRow {
                symbol: "hew_dyn_box_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapContainsKeyLayout => RuntimeOpRow {
                symbol: "hew_hashmap_contains_key_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapClearLayout => RuntimeOpRow {
                symbol: "hew_hashmap_clear_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapCloneLayout => RuntimeOpRow {
                symbol: "hew_hashmap_clone_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapEntriesLayout => RuntimeOpRow {
                symbol: "hew_hashmap_entries_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapFreeLayout => RuntimeOpRow {
                symbol: "hew_hashmap_free_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapGetLayout => RuntimeOpRow {
                symbol: "hew_hashmap_get_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashMapLayoutGet,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapInsertLayout => RuntimeOpRow {
                symbol: "hew_hashmap_insert_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapKeysLayout => RuntimeOpRow {
                symbol: "hew_hashmap_keys_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapLenLayout => RuntimeOpRow {
                symbol: "hew_hashmap_len_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapNew => RuntimeOpRow {
                symbol: "HashMap::new",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapNewWithLayout => RuntimeOpRow {
                symbol: "hew_hashmap_new_with_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapRemoveLayout => RuntimeOpRow {
                symbol: "hew_hashmap_remove_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashMapValuesLayout => RuntimeOpRow {
                symbol: "hew_hashmap_values_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetContainsLayout => RuntimeOpRow {
                symbol: "hew_hashset_contains_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetClearLayout => RuntimeOpRow {
                symbol: "hew_hashset_clear_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetCloneLayout => RuntimeOpRow {
                symbol: "hew_hashset_clone_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetFreeLayout => RuntimeOpRow {
                symbol: "hew_hashset_free_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetInsertLayout => RuntimeOpRow {
                symbol: "hew_hashset_insert_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetIsEmptyLayout => RuntimeOpRow {
                symbol: "hew_hashset_is_empty_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetLenLayout => RuntimeOpRow {
                symbol: "hew_hashset_len_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetNew => RuntimeOpRow {
                symbol: "HashSet::new",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetNewWithLayout => RuntimeOpRow {
                symbol: "hew_hashset_new_with_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetRemoveLayout => RuntimeOpRow {
                symbol: "hew_hashset_remove_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::HashSetToVecLayout => RuntimeOpRow {
                symbol: "hew_hashset_to_vec_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::HashCollectionLayoutOp,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::InstantDurationSince => declared::INSTANTDURATIONSINCE.row,
            Self::InstantElapsed => declared::INSTANTELAPSED.row,
            Self::InstantNow => declared::INSTANTNOW.row,
            Self::MathIntrinsic(MathIntrinsic::Sqrt) => RuntimeOpRow {
                symbol: "sqrt",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Exp) => RuntimeOpRow {
                symbol: "exp",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log) => RuntimeOpRow {
                symbol: "log",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Sin) => RuntimeOpRow {
                symbol: "sin",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Cos) => RuntimeOpRow {
                symbol: "cos",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::AbsI64) => RuntimeOpRow {
                symbol: "abs",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[RuntimeLogicalFailure::IntegerOverflow],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MinI64) => RuntimeOpRow {
                symbol: "min",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MaxI64) => RuntimeOpRow {
                symbol: "max",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::AbsF64) => RuntimeOpRow {
                symbol: "abs_f",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MinF64) => RuntimeOpRow {
                symbol: "min_f",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::MaxF64) => RuntimeOpRow {
                symbol: "max_f",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Pow) => RuntimeOpRow {
                symbol: "pow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Floor) => RuntimeOpRow {
                symbol: "floor",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Ceil) => RuntimeOpRow {
                symbol: "ceil",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Round) => RuntimeOpRow {
                symbol: "round",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Tan) => RuntimeOpRow {
                symbol: "tan",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Asin) => RuntimeOpRow {
                symbol: "asin",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Acos) => RuntimeOpRow {
                symbol: "acos",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Atan) => RuntimeOpRow {
                symbol: "atan",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Atan2) => RuntimeOpRow {
                symbol: "atan2",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Sinh) => RuntimeOpRow {
                symbol: "sinh",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Cosh) => RuntimeOpRow {
                symbol: "cosh",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Tanh) => RuntimeOpRow {
                symbol: "tanh",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Exp2) => RuntimeOpRow {
                symbol: "exp2",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log2) => RuntimeOpRow {
                symbol: "log2",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log10) => RuntimeOpRow {
                symbol: "log10",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Log1p) => RuntimeOpRow {
                symbol: "log1p",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Expm1) => RuntimeOpRow {
                symbol: "expm1",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Cbrt) => RuntimeOpRow {
                symbol: "cbrt",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Hypot) => RuntimeOpRow {
                symbol: "hypot",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Fma) => RuntimeOpRow {
                symbol: "fma",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Trunc) => RuntimeOpRow {
                symbol: "trunc",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Copysign) => RuntimeOpRow {
                symbol: "copysign",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::Powi) => RuntimeOpRow {
                symbol: "powi",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::F64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MathIntrinsic(MathIntrinsic::FromBits) => RuntimeOpRow {
                symbol: "from_bits",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::F64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::ToBits) => RuntimeOpRow {
                symbol: "to_bits",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsNan) => RuntimeOpRow {
                symbol: "is_nan",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsFinite) => RuntimeOpRow {
                symbol: "is_finite",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsInfinite) => RuntimeOpRow {
                symbol: "is_infinite",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::FloatMethod(FloatMethodOp::IsSignNegative) => RuntimeOpRow {
                symbol: "is_sign_negative",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "count_ones.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "count_ones.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "count_ones.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "count_ones.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "count_ones.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "count_ones.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "count_ones.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "count_ones.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "count_ones.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountOnes, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "count_ones.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "count_zeros.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "count_zeros.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "count_zeros.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "count_zeros.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "count_zeros.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "count_zeros.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "count_zeros.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "count_zeros.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "count_zeros.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::CountZeros, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "count_zeros.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "leading_zeros.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "leading_zeros.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "leading_zeros.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "leading_zeros.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "leading_zeros.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "leading_zeros.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "leading_zeros.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "leading_zeros.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "leading_zeros.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::LeadingZeros, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "leading_zeros.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "trailing_zeros.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "trailing_zeros.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "trailing_zeros.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "trailing_zeros.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "trailing_zeros.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "trailing_zeros.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "trailing_zeros.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "trailing_zeros.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "trailing_zeros.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::TrailingZeros, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "trailing_zeros.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "swap_bytes.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "swap_bytes.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "swap_bytes.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "swap_bytes.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "swap_bytes.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "swap_bytes.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "swap_bytes.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "swap_bytes.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "swap_bytes.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::SwapBytes, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "swap_bytes.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "reverse_bits.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "reverse_bits.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "reverse_bits.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "reverse_bits.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "reverse_bits.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Isize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "reverse_bits.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "reverse_bits.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U16,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "reverse_bits.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "reverse_bits.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::ReverseBits, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "reverse_bits.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Usize,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "rotate_left.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "rotate_left.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "rotate_left.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "rotate_left.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "rotate_left.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "rotate_left.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "rotate_left.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "rotate_left.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "rotate_left.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateLeft, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "rotate_left.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "rotate_right.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "rotate_right.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "rotate_right.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "rotate_right.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "rotate_right.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "rotate_right.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "rotate_right.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "rotate_right.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "rotate_right.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntMethod(IntBitOp::RotateRight, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "rotate_right.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "wrapping_add.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "wrapping_add.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "wrapping_add.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "wrapping_add.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "wrapping_add.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "wrapping_add.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "wrapping_add.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "wrapping_add.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "wrapping_add.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingAdd, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "wrapping_add.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "wrapping_sub.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "wrapping_sub.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "wrapping_sub.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "wrapping_sub.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "wrapping_sub.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "wrapping_sub.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "wrapping_sub.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "wrapping_sub.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "wrapping_sub.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingSub, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "wrapping_sub.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "wrapping_mul.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "wrapping_mul.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "wrapping_mul.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "wrapping_mul.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "wrapping_mul.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "wrapping_mul.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "wrapping_mul.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "wrapping_mul.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "wrapping_mul.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::WrappingMul, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "wrapping_mul.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "saturating_add.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "saturating_add.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "saturating_add.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "saturating_add.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "saturating_add.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "saturating_add.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "saturating_add.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "saturating_add.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "saturating_add.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingAdd, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "saturating_add.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "saturating_sub.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "saturating_sub.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "saturating_sub.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "saturating_sub.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "saturating_sub.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "saturating_sub.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "saturating_sub.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "saturating_sub.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "saturating_sub.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingSub, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "saturating_sub.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "saturating_mul.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "saturating_mul.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "saturating_mul.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "saturating_mul.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "saturating_mul.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Isize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "saturating_mul.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U8),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "saturating_mul.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U16),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "saturating_mul.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "saturating_mul.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::SaturatingMul, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "saturating_mul.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Usize),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "checked_add.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "checked_add.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "checked_add.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "checked_add.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "checked_add.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Isize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "checked_add.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "checked_add.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "checked_add.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "checked_add.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedAdd, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "checked_add.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Usize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "checked_sub.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "checked_sub.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "checked_sub.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "checked_sub.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "checked_sub.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Isize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "checked_sub.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "checked_sub.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "checked_sub.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "checked_sub.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedSub, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "checked_sub.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Usize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I8) => RuntimeOpRow {
                symbol: "checked_mul.i8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I16) => RuntimeOpRow {
                symbol: "checked_mul.i16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I32) => RuntimeOpRow {
                symbol: "checked_mul.i32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::I64) => RuntimeOpRow {
                symbol: "checked_mul.i64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::Isize) => RuntimeOpRow {
                symbol: "checked_mul.isize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Isize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Isize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U8) => RuntimeOpRow {
                symbol: "checked_mul.u8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U8,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U8])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U16) => RuntimeOpRow {
                symbol: "checked_mul.u16",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U16,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U16])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U32) => RuntimeOpRow {
                symbol: "checked_mul.u32",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U32,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U32])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::U64) => RuntimeOpRow {
                symbol: "checked_mul.u64",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::U64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::U64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::IntArith(IntArithKind::CheckedMul, IntMethodWidth::Usize) => RuntimeOpRow {
                symbol: "checked_mul.usize",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::Usize,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Usize])),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeAllowPeer => RuntimeOpRow {
                symbol: "Node::allow_peer",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeConnect => RuntimeOpRow {
                symbol: "Node::connect",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Result,
                        &[K::Unit, K::MonomorphicBuiltin(BuiltinType::NodeError)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NodeResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeIdDisplay => RuntimeOpRow {
                symbol: "hew_node_id_display",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::NodeId),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocationNodeId => RuntimeOpRow {
                symbol: "hew_location_node_id",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::Location),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::BuiltinNominal(BuiltinType::NodeId)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocationSlot => RuntimeOpRow {
                symbol: "hew_location_slot",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::Location),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocationIncarnation => RuntimeOpRow {
                symbol: "hew_location_incarnation",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::Location),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocationDisplay => RuntimeOpRow {
                symbol: "hew_location_display",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::Location),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RemotePidLocation => RuntimeOpRow {
                symbol: "hew_remote_pid_location",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::RemotePid),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::BuiltinNominal(BuiltinType::Location)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RemotePidNodeId => RuntimeOpRow {
                symbol: "hew_remote_pid_node_id",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::RemotePid),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::BuiltinNominal(BuiltinType::NodeId)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RemotePidSlot => RuntimeOpRow {
                symbol: "hew_remote_pid_slot",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::RemotePid),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::U64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RemotePidIncarnation => RuntimeOpRow {
                symbol: "hew_remote_pid_incarnation",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::RemotePid),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::U32),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RemotePidDisplay => RuntimeOpRow {
                symbol: "hew_remote_pid_display",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::BuiltinArgument(BuiltinType::RemotePid),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeId => RuntimeOpRow {
                symbol: "Node::id",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Option,
                        &[K::BuiltinNominal(BuiltinType::NodeId)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeIdentityKey => RuntimeOpRow {
                symbol: "Node::identity_key",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeLoadKeys => RuntimeOpRow {
                symbol: "Node::load_keys",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeLookup => RuntimeOpRow {
                symbol: "Node::lookup",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::NodeLookupResult),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NodeResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeMonitor => RuntimeOpRow {
                symbol: "hew_node_monitor_location",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeRegister => RuntimeOpRow {
                symbol: "Node::register",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::Receiver(BuiltinType::ActorHandle),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeSetTransport => RuntimeOpRow {
                symbol: "Node::set_transport",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeShutdown => RuntimeOpRow {
                symbol: "Node::shutdown",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::NodeStart => RuntimeOpRow {
                symbol: "Node::start",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Named("std.builtins.NodeConfig"),
                        effect: E::Move,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Result,
                        &[K::Unit, K::MonomorphicBuiltin(BuiltinType::NodeError)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NodeResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricCounterRegister => RuntimeOpRow {
                symbol: "hew_metric_counter_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricCounterInc => RuntimeOpRow {
                symbol: "hew_metric_counter_inc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricCounterAdd => RuntimeOpRow {
                symbol: "hew_metric_counter_add",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeRegister => RuntimeOpRow {
                symbol: "hew_metric_gauge_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeSet => RuntimeOpRow {
                symbol: "hew_metric_gauge_set",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeInc => RuntimeOpRow {
                symbol: "hew_metric_gauge_inc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeDec => RuntimeOpRow {
                symbol: "hew_metric_gauge_dec",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricGaugeAdd => RuntimeOpRow {
                symbol: "hew_metric_gauge_add",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricHistogramRegister => RuntimeOpRow {
                symbol: "hew_metric_histogram_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricHistogramRegisterSimple => RuntimeOpRow {
                symbol: "hew_metric_histogram_register_simple",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricHistogramRecord => RuntimeOpRow {
                symbol: "hew_metric_histogram_record",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricVecRegister => RuntimeOpRow {
                symbol: "hew_metric_vec_register",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::MetricVecWith => RuntimeOpRow {
                symbol: "hew_metric_vec_with",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveReadU64 => RuntimeOpRow {
                symbol: "hew_observe_read_u64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveScrape => RuntimeOpRow {
                symbol: "hew_observe_scrape",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveSeries => RuntimeOpRow {
                symbol: "hew_observe_series",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ObserveBarrier => RuntimeOpRow {
                symbol: "hew_observe_barrier",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            // --- Rc/Weak ownership -------------------------------------
            // A strong handle is the payload pointer; a weak handle is the
            // allocation header pointer. `Rc.new` hands the runtime the
            // payload's release recipe, so every later release of the last
            // strong reference runs the payload's own destructor. `Rc.drop`
            // and `Weak.drop` are destroy actions rather than call sites:
            // physical MIR reaches their symbols through `DestroyAction`.
            Self::RcNew => RuntimeOpRow {
                symbol: "hew_rc_new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::SharedPayload,
                        effect: E::Value,
                    }],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Rc)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::SharedHandle,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcClone => RuntimeOpRow {
                symbol: "hew_rc_clone",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Rc)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcDowngrade => RuntimeOpRow {
                symbol: "hew_rc_downgrade",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(BuiltinType::Weak, &[K::SharedPayload])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcDrop => RuntimeOpRow {
                symbol: "hew_rc_drop",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcGet => RuntimeOpRow {
                symbol: "hew_rc_get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::SharedPayload),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::SharedHandle,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcIsUnique => RuntimeOpRow {
                symbol: "hew_rc_is_unique",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthI32,
            },
            Self::RcSet => RuntimeOpRow {
                symbol: "hew_rc_set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        RuntimeArgumentContract {
                            ty: K::Receiver(BuiltinType::Rc),
                            effect: E::Borrow,
                        },
                        RuntimeArgumentContract {
                            ty: K::SharedPayload,
                            effect: E::Value,
                        },
                    ],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::SharedHandle,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcStrongCount => RuntimeOpRow {
                symbol: "hew_rc_strong_count",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RcWeakCount => RuntimeOpRow {
                symbol: "hew_rc_weak_count",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Rc),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::WeakCloneRc => RuntimeOpRow {
                symbol: "hew_weak_clone_rc",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Weak),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Weak)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::WeakDropRc => RuntimeOpRow {
                symbol: "hew_weak_drop_rc",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::WeakUpgradeRc => RuntimeOpRow {
                symbol: "hew_weak_upgrade_rc",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[RuntimeArgumentContract {
                        ty: K::Receiver(BuiltinType::Weak),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(
                        BuiltinType::Option,
                        &[K::Applied(BuiltinType::Rc, &[K::SharedPayload])],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexCapture => RuntimeOpRow {
                symbol: "hew_regex_capture",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexCompile => RuntimeOpRow {
                symbol: "hew_regex_compile",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexFreeCapture => RuntimeOpRow {
                symbol: "hew_regex_free_capture",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexHandle => RuntimeOpRow {
                symbol: "hew_regex_handle",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::BitCopy(K::NamedOpaque("std.text.regex.PatternHandle")),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::RegexMatch => RuntimeOpRow {
                symbol: "hew_regex_match",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyChannelCancel => RuntimeOpRow {
                symbol: "hew_reply_channel_cancel",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyChannelFree => RuntimeOpRow {
                symbol: "hew_reply_channel_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyChannelNew => RuntimeOpRow {
                symbol: "hew_reply_channel_new",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyPayloadFree => RuntimeOpRow {
                symbol: "hew_reply_payload_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ReplyWait => RuntimeOpRow {
                symbol: "hew_reply_wait",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SelectFirst => RuntimeOpRow {
                symbol: "hew_select_first",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkClone => RuntimeOpRow {
                symbol: "hew_sink_clone",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::PipeHalf(PipeHalfKind::Sink),
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::PipeHalf(PipeHalfKind::Sink)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkFinish => RuntimeOpRow {
                symbol: "hew_sink_finish",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::PipeHalf(PipeHalfKind::Sink),
                        effect: E::Borrow,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkClose => RuntimeOpRow {
                symbol: "hew_sink_close",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Sink),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SinkPeerClosed => RuntimeOpRow {
                symbol: "hew_sink_peer_closed",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamClose => RuntimeOpRow {
                symbol: "hew_stream_close",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Stream),
                        effect: E::Move,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamChunks => RuntimeOpRow {
                symbol: "hew_stream_chunks",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Stream),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Stream)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamForward => RuntimeOpRow {
                symbol: "hew_stream_pipe",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::PipeHalf(PipeHalfKind::Stream),
                            effect: E::Move,
                        },
                        A {
                            ty: K::PipeHalf(PipeHalfKind::Sink),
                            effect: E::Move,
                        },
                    ],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamPairSink => RuntimeOpRow {
                symbol: "hew_stream_pair_sink",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::StreamPair,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::PipeHalfResult(PipeHalfKind::Sink)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamPairStream => RuntimeOpRow {
                symbol: "hew_stream_pair_stream",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::StreamPair,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::PipeHalfResult(PipeHalfKind::Stream)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamLines => RuntimeOpRow {
                symbol: "hew_stream_lines",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Stream),
                        effect: E::Move,
                    }],
                    // `Stream<bytes>` in, `Stream<string>` out: the result
                    // takes its element from the checked expression type.
                    result: R::FreshOwned(K::PipeHalfResult(PipeHalfKind::Stream)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamTake => RuntimeOpRow {
                symbol: "hew_stream_take",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Stream),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Stream)),
                    failures: &[],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamNextLayout => RuntimeOpRow {
                symbol: "hew_stream_next_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamSendLayout => RuntimeOpRow {
                symbol: "hew_stream_send_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamTryNextLayout => RuntimeOpRow {
                symbol: "hew_stream_try_next_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StreamTrySendLayout => RuntimeOpRow {
                symbol: "hew_stream_try_send_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringCharAt => RuntimeOpRow {
                symbol: "hew_string_char_at",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::Char])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringCharAtUtf8 => RuntimeOpRow {
                symbol: "hew_string_char_at_utf8",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringCharCount => RuntimeOpRow {
                symbol: "hew_string_char_count",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringByteLen => RuntimeOpRow {
                symbol: "hew_string_byte_length",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringConcat => RuntimeOpRow {
                symbol: "hew_string_concat",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringEquals => RuntimeOpRow {
                symbol: "hew_string_equals",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthI32,
            },
            Self::StringCompare => RuntimeOpRow {
                symbol: "hew_string_compare",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::I32),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringStartsWith => RuntimeOpRow {
                symbol: "hew_string_starts_with",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringEndsWith => RuntimeOpRow {
                symbol: "hew_string_ends_with",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringContains => RuntimeOpRow {
                symbol: "hew_string_contains",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsEmpty => RuntimeOpRow {
                symbol: "hew_string_is_empty",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsDigit => RuntimeOpRow {
                symbol: "hew_string_is_digit",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsAlpha => RuntimeOpRow {
                symbol: "hew_string_is_alpha",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StringIsAlphanumeric => RuntimeOpRow {
                symbol: "hew_string_is_alphanumeric",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::Bool),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::TruthBool,
            },
            Self::StructuralFormat => RuntimeOpRow {
                // The render dispatches per type, so this names the operation
                // rather than a linker symbol: physical MIR selects the
                // operand's recipe tree and codegen emits one borrow-only
                // formatter thunk per participating type.
                symbol: "hew_structural_format",
                // Rendering reads the value's layout and allocates the text.
                // The receiver stays the caller's: `f"{holder:?}"` twice over
                // one binding renders twice and releases nothing.
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::StructuralOperand,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::StructuralFormat,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringFind => RuntimeOpRow {
                symbol: "hew_string_find",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(BuiltinType::Option, &[K::I64])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::VariantResult,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringIndex => RuntimeOpRow {
                symbol: "hew_string_index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::BitCopy(K::Char),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringLen => RuntimeOpRow {
                symbol: "hew_string_length",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringRepeat => RuntimeOpRow {
                symbol: "hew_string_repeat",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringReplace => RuntimeOpRow {
                symbol: "hew_string_replace",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringClone => RuntimeOpRow {
                symbol: "hew_string_clone",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSplit => RuntimeOpRow {
                symbol: "hew_string_split",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::Applied(BuiltinType::Vec, &[K::String])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringLines => RuntimeOpRow {
                symbol: "hew_string_lines",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(BuiltinType::Vec, &[K::String])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringChars => RuntimeOpRow {
                symbol: "hew_string_chars",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Applied(BuiltinType::Vec, &[K::Char])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSliceCodepoints => RuntimeOpRow {
                symbol: "hew_string_slice_codepoints",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSliceCodepointsFrom => RuntimeOpRow {
                symbol: "hew_string_slice_codepoints_from",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringSlice => RuntimeOpRow {
                symbol: "hew_string_slice",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringToLowercase => RuntimeOpRow {
                symbol: "hew_string_to_lowercase",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringToBytes => RuntimeOpRow {
                symbol: "hew_string_to_bytes",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::Bytes),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringToUppercase => RuntimeOpRow {
                symbol: "hew_string_to_uppercase",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StringTrim => RuntimeOpRow {
                symbol: "hew_string_trim",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::U8ToString => RuntimeOpRow {
                symbol: "hew_u8_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::I32ToString => RuntimeOpRow {
                symbol: "hew_int_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::I64ToString => RuntimeOpRow {
                symbol: "hew_i64_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::U32ToString => RuntimeOpRow {
                symbol: "hew_uint_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::U64ToString => RuntimeOpRow {
                symbol: "hew_u64_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::F64ToString => RuntimeOpRow {
                symbol: "hew_float_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::CharToString => RuntimeOpRow {
                symbol: "hew_char_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Char,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I32,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I32,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I64,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::I64,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U8,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U8,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U8,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U32,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U32,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U32,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U64,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::U64,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::U64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::F64,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::F64,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::F64,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Bool,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bool,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Bool,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bool,
                        effect: E::Copy,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Str,
                newline: false,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Print {
                kind: PrintKind::Str,
                newline: true,
            } => RuntimeOpRow {
                symbol: "hew_print_value",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::ProcessExit => RuntimeOpRow {
                symbol: "hew_exit",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::I64,
                        effect: E::Copy,
                    }],
                    result: R::Never,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::StderrWrite => RuntimeOpRow {
                symbol: "hew_io_write_err",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::String,
                        effect: E::Borrow,
                    }],
                    result: R::Unit,
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::BoolToString => RuntimeOpRow {
                symbol: "hew_bool_to_string",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Bool,
                        effect: E::Copy,
                    }],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Direct,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorDirectId => RuntimeOpRow {
                symbol: "hew_supervisor_direct_id",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorChildGet => RuntimeOpRow {
                symbol: "hew_supervisor_child_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocalPidSupervisorChildGet => RuntimeOpRow {
                symbol: "hew_local_pid_supervisor_child_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorNestedGet => RuntimeOpRow {
                symbol: "hew_supervisor_nested_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorPoolChildGet => RuntimeOpRow {
                symbol: "hew_supervisor_pool_child_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::LocalPidSupervisorPoolChildRefGet => RuntimeOpRow {
                symbol: "hew_local_pid_supervisor_pool_child_ref_get",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorPoolLen => RuntimeOpRow {
                symbol: "hew_supervisor_pool_len",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorStop => RuntimeOpRow {
                symbol: "hew_supervisor_stop",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::SupervisorRestartAwaitBlocking => RuntimeOpRow {
                symbol: "hew_supervisor_restart_await_blocking",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::TcpAttachLocal => declared::TCPATTACHLOCAL.row,
            Self::TlsAttachLocal => declared::TLSATTACHLOCAL.row,
            Self::WebSocketAttachLocal => declared::WEBSOCKETATTACHLOCAL.row,
            Self::TaskFree => RuntimeOpRow {
                symbol: "hew_task_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::GeneratorFree => RuntimeOpRow {
                symbol: "hew_checked_generator_free",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::New) => RuntimeOpRow {
                symbol: "vec.value.new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Len) => RuntimeOpRow {
                symbol: "vec.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Contains) => RuntimeOpRow {
                symbol: "vec.value.contains",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Index) => RuntimeOpRow {
                symbol: "vec.value.index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::TypeArgument(0)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Get) => RuntimeOpRow {
                symbol: "vec.value.get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Option,
                        &[K::TypeArgument(0)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Push) => RuntimeOpRow {
                symbol: "vec.value.push",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Set) => RuntimeOpRow {
                symbol: "vec.value.set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Pop) => RuntimeOpRow {
                symbol: "vec.value.pop",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::Vec),
                        K::TypeArgument(0),
                    ])),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Remove) => RuntimeOpRow {
                symbol: "vec.value.remove",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::Vec),
                        K::TypeArgument(0),
                    ])),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::TakeAll) => RuntimeOpRow {
                symbol: "vec.value.take_all",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::Vec),
                        K::Receiver(BuiltinType::Vec),
                    ])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Clear) => RuntimeOpRow {
                symbol: "vec.value.clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::IndexBorrow) => RuntimeOpRow {
                symbol: "vec.value.index_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::Borrowed(K::TypeArgument(0)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::GetBorrow) => RuntimeOpRow {
                symbol: "vec.value.get_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::Borrowed(K::Applied(BuiltinType::Option, &[K::TypeArgument(0)])),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::TakeFirst) => RuntimeOpRow {
                symbol: "vec.value.take_first",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::Vec),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::Vec),
                        K::TypeArgument(0),
                    ])),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Slice) => RuntimeOpRow {
                symbol: "vec.value.slice",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Receiver(BuiltinType::Vec)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::SliceFrom) => RuntimeOpRow {
                symbol: "vec.value.slice_from",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::Receiver(BuiltinType::Vec)),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Append) => RuntimeOpRow {
                symbol: "vec.value.append",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Move,
                        },
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::Vec)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Vector(VecValueOp::Join) => RuntimeOpRow {
                symbol: "vec.value.join",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::Vec),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::String,
                            effect: E::Borrow,
                        },
                    ],
                    result: R::FreshOwned(K::String),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::Len) => RuntimeOpRow {
                symbol: "array.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::FixedArray,
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::Index) => RuntimeOpRow {
                symbol: "array.value.index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::FixedArray,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::IndependentValue(K::ArrayElement),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::IndexBorrow) => RuntimeOpRow {
                symbol: "array.value.index_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::FixedArray,
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                    ],
                    result: R::Borrowed(K::ArrayElement),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Array(ArrayValueOp::Set) => RuntimeOpRow {
                symbol: "array.value.set",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::FixedArray,
                            effect: E::Move,
                        },
                        A {
                            ty: K::I64,
                            effect: E::Copy,
                        },
                        A {
                            ty: K::ArrayElement,
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::FixedArray),
                    failures: &[RuntimeLogicalFailure::IndexOutOfBounds],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Vector,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::New) => RuntimeOpRow {
                symbol: "map.value.new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Receiver(BuiltinType::HashMap)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Len) => RuntimeOpRow {
                symbol: "map.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Index) => RuntimeOpRow {
                symbol: "map.value.index",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::IndependentValue(K::TypeArgument(1)),
                    failures: &[
                        RuntimeLogicalFailure::CallbackFault,
                        RuntimeLogicalFailure::IndexOutOfBounds,
                    ],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Get) => RuntimeOpRow {
                symbol: "map.value.get",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Option,
                        &[K::TypeArgument(1)],
                    )),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::GetBorrow) => RuntimeOpRow {
                symbol: "map.value.get_borrow",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::Borrowed(K::Applied(BuiltinType::Option, &[K::TypeArgument(1)])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::ContainsKey) => RuntimeOpRow {
                symbol: "map.value.contains_key",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Insert) => RuntimeOpRow {
                symbol: "map.value.insert",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(1),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::HashMap)),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Remove) => RuntimeOpRow {
                symbol: "map.value.remove",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashMap),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::HashMap),
                        K::Applied(BuiltinType::Option, &[K::TypeArgument(1)]),
                    ])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Clear) => RuntimeOpRow {
                symbol: "map.value.clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::HashMap)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Keys) => RuntimeOpRow {
                symbol: "map.value.keys",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::TypeArgument(0)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Values) => RuntimeOpRow {
                symbol: "map.value.values",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::TypeArgument(1)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Map(MapValueOp::Entries) => RuntimeOpRow {
                symbol: "map.value.entries",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashMap),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::Tuple(&[K::TypeArgument(0), K::TypeArgument(1)])],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Map,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::New) => RuntimeOpRow {
                symbol: "set.value.new",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[],
                    result: R::FreshOwned(K::Receiver(BuiltinType::HashSet)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Len) => RuntimeOpRow {
                symbol: "set.value.len",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashSet),
                        effect: E::Borrow,
                    }],
                    result: R::BitCopy(K::I64),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Contains) => RuntimeOpRow {
                symbol: "set.value.contains",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashSet),
                            effect: E::Borrow,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::BitCopy(K::Bool),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Insert) => RuntimeOpRow {
                symbol: "set.value.insert",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashSet),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Value,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::HashSet),
                        K::Bool,
                    ])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Remove) => RuntimeOpRow {
                symbol: "set.value.remove",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[
                        A {
                            ty: K::Receiver(BuiltinType::HashSet),
                            effect: E::Move,
                        },
                        A {
                            ty: K::TypeArgument(0),
                            effect: E::Borrow,
                        },
                    ],
                    result: R::UpdatedReceiverAndValue(K::Tuple(&[
                        K::Receiver(BuiltinType::HashSet),
                        K::Bool,
                    ])),
                    failures: &[RuntimeLogicalFailure::CallbackFault],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Clear) => RuntimeOpRow {
                symbol: "set.value.clear",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashSet),
                        effect: E::Move,
                    }],
                    result: R::UpdatedReceiver(K::Receiver(BuiltinType::HashSet)),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::Set(SetValueOp::Elements) => RuntimeOpRow {
                symbol: "set.value.elements",
                contract: Some(RuntimeSemanticContract {
                    arguments: &[A {
                        ty: K::Receiver(BuiltinType::HashSet),
                        effect: E::Borrow,
                    }],
                    result: R::IndependentValue(K::Applied(
                        BuiltinType::Vec,
                        &[K::TypeArgument(0)],
                    )),
                    failures: &[],
                }),
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::Set,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecAppend => RuntimeOpRow {
                symbol: "hew_vec_append",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecClear => RuntimeOpRow {
                symbol: "hew_vec_clear",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecClone => RuntimeOpRow {
                symbol: "hew_vec_clone",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecCloneLayout => RuntimeOpRow {
                symbol: "hew_vec_clone_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecCloneOwned => RuntimeOpRow {
                symbol: "hew_vec_clone_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsLayout => RuntimeOpRow {
                symbol: "hew_vec_contains_thunk",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecTakeAll => RuntimeOpRow {
                symbol: "hew_vec_take_all",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsOwned => RuntimeOpRow {
                symbol: "hew_vec_contains_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::F64) => RuntimeOpRow {
                symbol: "hew_vec_contains_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::I32) => RuntimeOpRow {
                symbol: "hew_vec_contains_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::I64) => RuntimeOpRow {
                symbol: "hew_vec_contains_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecContainsScalar(VecContainsScalarElem::Str) => RuntimeOpRow {
                symbol: "hew_vec_contains_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Bool) => RuntimeOpRow {
                symbol: "hew_vec_get_bool",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::F32) => RuntimeOpRow {
                symbol: "hew_vec_get_f32",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::F64) => RuntimeOpRow {
                symbol: "hew_vec_get_f64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I8) => RuntimeOpRow {
                symbol: "hew_vec_get_i8",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I16) => RuntimeOpRow {
                symbol: "hew_vec_get_i16",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I32) => RuntimeOpRow {
                symbol: "hew_vec_get_i32",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecI32GetSet,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::I64) => RuntimeOpRow {
                symbol: "hew_vec_get_i64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Clone) => RuntimeOpRow {
                symbol: "hew_vec_get_clone",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Take) => RuntimeOpRow {
                symbol: "hew_vec_take_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Layout) => RuntimeOpRow {
                symbol: "hew_vec_get_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Owned) => RuntimeOpRow {
                symbol: "hew_vec_get_owned",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Ptr) => RuntimeOpRow {
                symbol: "hew_vec_get_ptr",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::Str) => RuntimeOpRow {
                symbol: "hew_vec_get_str",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::U8) => RuntimeOpRow {
                symbol: "hew_vec_get_u8",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecGet(VecGetElem::U16) => RuntimeOpRow {
                symbol: "hew_vec_get_u16",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecIsEmpty => RuntimeOpRow {
                symbol: "hew_vec_is_empty",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecJoinStr => RuntimeOpRow {
                symbol: "hew_vec_join_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecLen => RuntimeOpRow {
                symbol: "hew_vec_len",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecNew => RuntimeOpRow {
                symbol: "Vec::new",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecConstructor,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPopBool => RuntimeOpRow {
                symbol: "hew_vec_pop_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPopLayout => RuntimeOpRow {
                symbol: "hew_vec_pop_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPopOwned => RuntimeOpRow {
                symbol: "hew_vec_pop_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushBool => RuntimeOpRow {
                symbol: "hew_vec_push_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushLayout => RuntimeOpRow {
                symbol: "hew_vec_push_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushOwned => RuntimeOpRow {
                symbol: "hew_vec_push_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecPushOwnedMove => RuntimeOpRow {
                symbol: "hew_vec_push_owned_move",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Push,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_push_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Pop,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_pop_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecI32GetSet,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::Set,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_set_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::F32,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_f32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::F64,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_f64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I8,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I16,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I32,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i32",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::I64,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_i64",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::Ptr,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_ptr",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::Str,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_str",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::U8,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_u8",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecScalar {
                op: VecScalarOp::RemoveAt,
                elem: VecScalarElem::U16,
            } => RuntimeOpRow {
                symbol: "hew_vec_remove_at_u16",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecScalarDirect,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecRemoveAtBool => RuntimeOpRow {
                symbol: "hew_vec_remove_at_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecRemoveAtLayout => RuntimeOpRow {
                symbol: "hew_vec_remove_at_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecRemoveAtOwned => RuntimeOpRow {
                symbol: "hew_vec_remove_at_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetBool => RuntimeOpRow {
                symbol: "hew_vec_set_bool",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecBool,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetLayout => RuntimeOpRow {
                symbol: "hew_vec_set_layout",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetOwned => RuntimeOpRow {
                symbol: "hew_vec_set_owned",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSetOwnedMove => RuntimeOpRow {
                symbol: "hew_vec_set_owned_move",
                contract: None,
                staging: RuntimeStaging::PreStaged,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Bytesize) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_bytesize",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::F64) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_f64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::I32) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_i32",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::I64) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_i64",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Layout) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_layout",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecLayout,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Owned) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_owned",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::VecOwned,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Ptr) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_ptr",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VecSliceRange(VecSliceElem::Str) => RuntimeOpRow {
                symbol: "hew_vec_slice_range_str",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
            Self::VtableDispatchPanicOnOob => RuntimeOpRow {
                symbol: "hew_vtable_dispatch_panic_on_oob",
                contract: None,
                staging: RuntimeStaging::Declared,
                abi_shape: RuntimeCallAbiShape::Other,
                physical: RuntimePhysicalForm::NotAnAction,
                c_return: RuntimeCReturn::Storage,
            },
        }
    }
}
