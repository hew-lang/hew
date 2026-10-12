# std.crypto.jwt

std.crypto.jwt — JWT encoding and validation

Part of the [Hew](https://hew.sh) standard library. See the [std overview](../../README.md) for all modules.

Verified decoding requires a valid `exp` claim and applies no clock-skew
leeway to `exp` or `nbf`. HMAC secrets must contain at
least 32 bytes for HS256, 48 for HS384 or 64 for HS512, and should be generated
with a cryptographically secure random source. At an authentication
boundary, also bind the token to its expected issuer and audience:

```hew
import std.crypto.jwt;

fn verify(token: string, secret: string) -> bool {
    jwt.validate_with_policy(
        token,
        secret,
        "HS256",
        "https://issuer.example",
        "hew-service",
    )
}
```

`decode_insecure` only inspects claims. It does not authenticate the token or
validate any claim and must not drive authorization decisions.
