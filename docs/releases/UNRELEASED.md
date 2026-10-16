# Unreleased

Node identity files now use secure exclusive publication and durable writes. Existing
files must be regular, owned by the current user and private to that user; symbolic
links, reparse points, unsafe permissions and mismatched public/private keys refuse.
Concurrent first-time callers retain one identity. Tighten an existing file's mode or
ACL instead of deleting the key: regenerating it changes the node's identity and pins.
Mesh identity clones share zeroizing private-key storage and redact diagnostic output.

The obsolete `HEW_TRANSPORT=quic` node transport is removed. Use `tcp` or `quic-mesh`
with the existing peer authentication configuration. `std.net.quic` remains available
as the ordinary QUIC socket API.
