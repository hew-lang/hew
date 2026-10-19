//! Trusted publishing from GitHub Actions: `hew publish --auth github-actions`.
//!
//! The publish process generates a signing key in memory, asks the runner
//! for an OIDC token whose audience is the registry's origin, and exchanges
//! the token and public key for a short-lived publish grant. Nothing is read
//! from or written to `credentials.toml` or `keys/`. When the grant expires
//! mid-run (the registry answers 401), a fresh OIDC token is exchanged again
//! for the same key.

use crate::client::RegistryClient;
use crate::publish::{Auth, PublishError, Signer};
use crate::signing::KeyPair;

/// The runner's OIDC token endpoint, from `ACTIONS_ID_TOKEN_REQUEST_URL` and
/// `ACTIONS_ID_TOKEN_REQUEST_TOKEN`.
#[derive(Debug, Clone)]
pub struct OidcEndpoint {
    /// `ACTIONS_ID_TOKEN_REQUEST_URL`.
    pub url: String,
    /// `ACTIONS_ID_TOKEN_REQUEST_TOKEN`.
    pub token: String,
}

impl OidcEndpoint {
    /// Read the endpoint from the environment a GitHub Actions job provides
    /// when it has `permissions: id-token: write`.
    ///
    /// # Errors
    ///
    /// Returns the diagnostic when either variable is missing.
    pub fn from_env() -> Result<Self, String> {
        let read = |name: &str| std::env::var(name).ok().filter(|value| !value.is_empty());
        match (
            read("ACTIONS_ID_TOKEN_REQUEST_URL"),
            read("ACTIONS_ID_TOKEN_REQUEST_TOKEN"),
        ) {
            (Some(url), Some(token)) => Ok(Self { url, token }),
            _ => Err(
                "`--auth github-actions` needs a workflow with `permissions: id-token: write`; \
                 ACTIONS_ID_TOKEN_REQUEST_URL and ACTIONS_ID_TOKEN_REQUEST_TOKEN are not set"
                    .to_string(),
            ),
        }
    }
}

/// Credentials from a GitHub Actions OIDC exchange, held only in memory.
#[derive(Debug)]
pub struct GithubActions {
    client: RegistryClient,
    /// The endpoint, or why the environment lacks one; only an upload needs it.
    endpoint: Result<OidcEndpoint, String>,
}

impl GithubActions {
    /// Exchange against the registry `client` addresses, fetching OIDC tokens
    /// from `endpoint`. A missing endpoint fails only when something uploads.
    #[must_use]
    pub fn new(client: RegistryClient, endpoint: Result<OidcEndpoint, String>) -> Self {
        Self { client, endpoint }
    }

    /// Fetch a fresh OIDC token and exchange it for a grant bound to
    /// `keypair`, returning the grant's token.
    fn exchange(&self, keypair: &KeyPair) -> Result<String, PublishError> {
        let fail = |step: &str, error: &dyn std::fmt::Display| {
            PublishError::Auth(format!(
                "GitHub Actions trusted publishing: {step}: {error}"
            ))
        };
        let endpoint = self
            .endpoint
            .as_ref()
            .map_err(|error| PublishError::Auth(error.clone()))?;
        let audience = self
            .client
            .api_origin()
            .map_err(|error| fail("registry origin", &error))?;
        let oidc_token = self
            .client
            .github_actions_oidc_token(&endpoint.url, &endpoint.token, &audience)
            .map_err(|error| fail("OIDC token request", &error))?;
        let grant = self
            .client
            .login_github_actions(&oidc_token, &keypair.public_key_base64())
            .map_err(|error| fail("token exchange", &error))?;
        let fingerprint = keypair.fingerprint();
        if grant.key_fingerprint != fingerprint {
            return Err(PublishError::Auth(format!(
                "E_AUTH_GRANT_KEY: the registry granted key {}, but this run signs with {fingerprint}",
                grant.key_fingerprint
            )));
        }
        println!(
            "authenticated as {} through GitHub Actions (key {fingerprint}, {} s)",
            grant.identity, grant.expires_in
        );
        Ok(grant.token)
    }
}

impl Auth for GithubActions {
    fn authenticate(&mut self) -> Result<Signer, PublishError> {
        let keypair = KeyPair::generate();
        let token = self.exchange(&keypair)?;
        Ok(Signer { keypair, token })
    }

    fn refresh(&mut self, keypair: &KeyPair) -> Result<Option<String>, PublishError> {
        self.exchange(keypair).map(Some)
    }
}
