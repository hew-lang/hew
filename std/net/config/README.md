# Node configuration

Import `std.net` and call `NodeConfig.load()` to read `[cluster]` in
`hew.toml` and apply `HEW_NODE_*` overrides. The example uses environment-only
configuration so it runs without an existing `hew.toml`:

```hew
import std.net;
import std.os;

fn main() {
    os.set_env("HEW_NODE_BIND", "127.0.0.1:9000").expect("set node bind");
    os.set_env("HEW_NODE_KEY", "node.key").expect("set node key");
    let _config = NodeConfig.load().expect("load node configuration");
}
```

Pass the loaded configuration to `Node.start` to start networking. That call
loads or creates the identity file named by `cluster.key` or `HEW_NODE_KEY`.

Set `HEW_NODE_CONFIG` to select another file. `bind` and `key` are required;
`transport` defaults to `tcp`, `cluster` defaults to an empty path, and `seeds`
defaults to an empty list.
Environment overrides are `HEW_NODE_BIND`, `HEW_NODE_KEY`, `HEW_NODE_TRANSPORT`,
`HEW_NODE_CLUSTER` and `HEW_NODE_SEEDS`. Seeds use TOML arrays, including `[]`
to clear the list. File identity and enrolment paths are relative to the
configuration directory; environment paths are relative to the working directory.

Errors retain their `source`, `field` and `message` in `ConfigError`, with a
human-readable `Display` implementation. Configuration loading never creates
an identity or starts networking. Use `hew node identity --key node.key` to
load or create the same TCP identity used by `Node.start` and print its public
credential. Add `--transport quic-mesh` for a QUIC credential.

Before connecting nodes, enrol their public credentials under one authority:

```sh
hew node authority --key authority.key
hew node identity --key node.key
hew node admit --authority authority.key --credential PUBLIC_CREDENTIAL --output node.enrollment
```

Set `cluster = "node.enrollment"` in `[cluster]` or set `HEW_NODE_CLUSTER`.
The installed enrolment supplies the trusted cluster authority and binds the
local transport credential. Every peer proves possession of its enrolled key.
An empty cluster path permits isolated local use, with peer admission refused.
Keep `authority.key` with the operator; nodes need only their own identity and
enrolment files. Actor service grants remain separate from cluster membership.
