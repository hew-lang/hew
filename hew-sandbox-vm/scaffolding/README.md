# Sandbox package validation

`validate-sandbox-vm.mjs` checks the package and trace schemas, source-fixture
catalog and documentation links. `record-trace.mjs` executes a compiled source
package and writes its deterministic trace.

Use `make sandbox-fixtures-record` and `make sandbox-fixtures-check` from the
repository root. Runtime admission is implemented by the VM loader; these
repository checks do not decide which language features can execute.
