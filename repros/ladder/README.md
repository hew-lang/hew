# Ladder probes

The `.hew` programs `docs/internal/ir-ladder.md` cites for its `[current]`
claims: every transcript that document quotes was produced by running one of
these. They lived in `.tmp/` while the ladder was being written, which made
none of the quoted behaviour reproducible from a checkout; they are committed
here so a P-lane implementer or a reviewer can re-run the exact program a
sentence rests on.

They are **probes, not fixtures**. No gate globs this directory, none carries a
`.expected`, and several are expected to be _rejected_ — that is the claim being
made. Run one with `hew run repros/ladder/<name>.hew` (with `TMPDIR` outside the
checkout). The transcripts the ladder quotes were produced on `hew
0.6.0-rc3-dev.141+fa2986bb2`, except `state_reinit.hew` and `dyn_rc.hew`, which
were run on `hew 0.6.0-rc3-dev.142+54e8dde2c` and re-reproduced on that binary
when they were committed here (`close 1` / `close 1` / `2` / `after` / `close 2`
and `5` / `alive` respectively).

A probe whose behaviour changes is not automatically a regression: several of
these record what `main` does **before** a §11 row changes it. Read the citing
section first.

Per the matrix's citation rule, **the section name is the citation and the line
is a convenience** — re-grep the line if it has drifted.

| probe                      | cited by `docs/internal/ir-ladder.md`                              |
| -------------------------- | ------------------------------------------------------------------ |
| `bind_copy.hew`            | §11 (L2712)                                                        |
| `borrowmut_capture.hew`    | §1.3.5 (L482); §11 (L2726)                                         |
| `bytes_clone.hew`          | §11 (L2718)                                                        |
| `cap_move.hew`             | §1.6 (L991); §1.6 (L1007)                                          |
| `cap_nomove.hew`           | §2.1 (L1241); §11 (L2732)                                          |
| `closure_borrow_conn.hew`  | §1.3.5 (L540); §11 (L2740)                                         |
| `closure_mut_share.hew`    | §1.3.5 (L500); §1.6 (L994); §2.1 (L1247); §7 (L2549); §11 (L2716)  |
| `closure_rebind.hew`       | §5.4 (L1892)                                                       |
| `cond_init.hew`            | §2.1 (L1155); §11 (L2710)                                          |
| `dyn_rc.hew`               | §3 (L1339); §11 (L2720)                                            |
| `fork_unawaited.hew`       | §2.1 (L1280); §11 (L2710)                                          |
| `gen_rebind.hew`           | §2.1 (L1103); §11 (L2710)                                          |
| `lambda_send_twice.hew`    | §1.1 (L192); §1.3.1 (L371); §1.6 (L991); §2.1 (L1179); §11 (L2714) |
| `let_field_mut.hew`        | §2.1 (L1209)                                                       |
| `let_index_assign.hew`     | §2.1 (L1210)                                                       |
| `let_map_insert.hew`       | §2.1 (L1204)                                                       |
| `let_state_push.hew`       | §1.3.6 (L600); §2.1 (L1204); §11 (L2711)                           |
| `linear_actor_field.hew`   | §1.3.6 (L689); §1.6 (L997); §11 (L2730)                            |
| `mutate_let.hew`           | §2.1 (L1203); §11 (L2711)                                          |
| `rc_clone.hew`             | §2.1 (L1218); §11 (L2713)                                          |
| `res_param_borrow.hew`     | §2.1 (L1118)                                                       |
| `res_param_consume.hew`    | §1.6 (L996); §2.1 (L1120); §4.2 (L1446); §11 (L2728)               |
| `resource_early_close.hew` | §2.1 (L1104); §4.2 (L1419); §11 (L2710)                            |
| `resource_keep.hew`        | §5.6 (L1936); §11 (L2733)                                          |
| `resource_send.hew`        | §11 (L2714)                                                        |
| `resource_send2.hew`       | §2.1 (L1179)                                                       |
| `state_alias.hew`          | §11 (L2735)                                                        |
| `state_reinit.hew`         | §1.3.6 (L667); §11 (L2744)                                         |
| `state_resource_mut.hew`   | §1.3.6 (L611)                                                      |
| `state_resource_trait.hew` | §1.3.6 (L610); §1.6 (L995); §11 (L2744)                            |
| `temp_close.hew`           | §1.3.4 (L449); §11 (L2736)                                         |
| `vec_rc_weak.hew`          | §1.1 (L183); §3 (L1339); §11 (L2720)                               |
| `vec_resource.hew`         | §1.1 (L183); §2.1 (L1105); §11 (L2712)                             |
| `vec_resource_drop.hew`    | §1.1 (L183); §1.3 (L287); §3 (L1339); §11 (L2713)                  |
| `weak_scope.hew`           | §3 (L1339); §11 (L2720)                                            |
