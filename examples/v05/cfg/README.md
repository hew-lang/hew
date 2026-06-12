# CFG-shape accept fixtures

These `.hew` sources exercise the basic-block structure introduced
by real CFG construction for `if`/`else`. Each compiles cleanly
through `hew compile`; the resulting binary returns a value
that names which arm was taken.

| Fixture | Returns | Asserts |
| --- | --- | --- |
| `if_returns_int_constant.hew` | `7` from the taken arm | smoke: bool literal + comparison condition + arm Move into result local + join Return |
| `nested_if_in_let_init.hew` | `42` | sequential `let r = if ...; ...` Ifs each contribute a 3-block split; the cursor monotonically advances |

The inline-source counterparts live in `hew-mir/tests/vertical.rs` as
`if_expression_builds_four_blocks`,
`if_expression_entry_block_terminates_with_branch`,
`if_expression_arm_blocks_goto_join`,
`if_expression_arm_blocks_write_result_local`,
`if_expression_join_block_terminates_with_return`,
`if_no_else_unit_typed_lowers_without_diagnostic`,
`sequential_ifs_each_contribute_three_blocks`. They pin the CFG
shape at the MIR layer; the `.hew` files here document the source
form and double as targets for manual `hew compile` runs.
