# Node configuration

Import `std.net` and call `NodeConfig.load()` to read `[cluster]` in `hew.toml`
and apply `HEW_NODE_*` overrides. The result can be passed directly to
`Node.start`:

```hew
import std.net;

fn main() {
    let config = NodeConfig.load().expect("load cluster configuration");
    Node.start(config).expect("start node");
    Node.shutdown();
}
```

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
