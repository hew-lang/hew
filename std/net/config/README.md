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
`transport` defaults to `tcp`, and `peers` and `seeds` default to empty lists.
Environment overrides are `HEW_NODE_BIND`, `HEW_NODE_KEY`, `HEW_NODE_TRANSPORT`,
`HEW_NODE_PEERS` and `HEW_NODE_SEEDS`. Lists use TOML arrays, including `[]` to
clear a list. File key paths are relative to the configuration directory;
environment key paths are relative to the working directory.

Errors retain their `source`, `field` and `message` in `ConfigError`, with a
human-readable `Display` implementation. Configuration loading never creates
an identity or starts networking. Use `hew node identity --key node.key` to
load or create the same TCP identity used by `Node.start` and print its public
credential. Add `--transport quic-mesh` for a QUIC credential.
