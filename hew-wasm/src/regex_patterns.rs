//! Regex literal matching for the browser VM, using the native regex dialect.

use wasm_bindgen::prelude::*;

/// Compiled once per literal in a running sandbox package.
#[wasm_bindgen]
#[derive(Debug)]
pub struct SandboxRegex(regex::Regex);

/// Compile a regex literal with the same engine as the type checker and native runtime.
///
/// # Errors
///
/// Returns the engine diagnostic for an invalid pattern.
#[wasm_bindgen(js_name = compileSandboxRegex)]
pub fn compile_sandbox_regex(pattern: &str) -> Result<SandboxRegex, JsError> {
    regex::Regex::new(pattern)
        .map(SandboxRegex)
        .map_err(|error| JsError::new(&error.to_string()))
}

#[wasm_bindgen]
impl SandboxRegex {
    #[wasm_bindgen(js_name = isMatch)]
    #[must_use]
    pub fn is_match(&self, text: &str) -> bool {
        self.0.is_match(text)
    }

    /// UTF-16 start/end pairs index JavaScript strings. Negative offsets mark
    /// an absent group. Convert all endpoints in one forward scan of the input.
    #[wasm_bindgen(js_name = captureSpans)]
    #[must_use]
    pub fn capture_spans(&self, text: &str) -> Vec<i64> {
        let Some(captures) = self.0.captures(text) else {
            return Vec::new();
        };
        let mut spans = vec![-1; captures.len() * 2];
        let mut endpoints = Vec::with_capacity(spans.len());
        for (index, group) in captures.iter().enumerate() {
            if let Some(group) = group {
                endpoints.push((group.start(), index * 2));
                endpoints.push((group.end(), index * 2 + 1));
            }
        }
        endpoints.sort_unstable();
        let mut previous = 0;
        let mut utf16 = 0;
        for (offset, slot) in endpoints {
            utf16 += text[previous..offset]
                .encode_utf16()
                .fold(0_i64, |count, _| count + 1);
            previous = offset;
            spans[slot] = utf16;
        }
        spans
    }
}
