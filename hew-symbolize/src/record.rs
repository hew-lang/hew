//! The failure records the runtime writes to `HEW_FAILURE_LOG`, one JSON
//! object per line, and their symbolization.
//!
//! Every field the runtime may leave out is optional and fields this reader
//! does not name are kept, so a record survives a read and rewrite intact.

use serde::{Deserialize, Serialize};
use serde_json::{Map, Value};

use crate::{BuildId, Frame, Location, Result, Symbolizer};

/// One failure record.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FailureRecordJson {
    /// The failure kind, such as `DivideByZero`.
    #[serde(default)]
    pub kind: Option<String>,
    /// The failure code.
    #[serde(default)]
    pub code: Option<i32>,
    #[serde(default)]
    pub message: Option<String>,
    /// Where the failure was raised.
    #[serde(default)]
    pub site: Option<SiteJson>,
    /// Return addresses captured at the origin, innermost first.
    #[serde(default)]
    pub frames: Vec<FrameJson>,
    #[serde(default)]
    pub build: Option<BuildJson>,
    /// Present when the failure arrived as a signal or exception; frame zero
    /// is then the faulting instruction rather than a return address.
    #[serde(default)]
    pub signal: Option<Value>,
    #[serde(flatten)]
    pub rest: Map<String, Value>,
}

/// The source position a failure was raised at.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SiteJson {
    #[serde(default)]
    pub file: Option<String>,
    #[serde(default)]
    pub line: Option<u32>,
    #[serde(default)]
    pub col: Option<u32>,
    #[serde(flatten)]
    pub rest: Map<String, Value>,
}

/// One captured frame.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FrameJson {
    /// The address, as `0x`-prefixed hex.
    pub pc: String,
    /// The Hew function the runtime's own tables named.
    #[serde(default)]
    pub hew_fn: Option<String>,
    /// The function the symbol files named, for frames the runtime left
    /// unnamed: runtime and foreign code, or Hew code without tables.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub function: Option<String>,
    #[serde(default)]
    pub file: Option<String>,
    #[serde(default)]
    pub line: Option<u32>,
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub col: Option<u32>,
    #[serde(flatten)]
    pub rest: Map<String, Value>,
}

/// Facts about the executable that wrote the record.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct BuildJson {
    /// The image's runtime base, as `0x`-prefixed hex.
    #[serde(default)]
    pub image_base: Option<String>,
    /// The image's build identity, as hex.
    #[serde(default)]
    pub build_id: Option<String>,
    #[serde(flatten)]
    pub rest: Map<String, Value>,
}

impl FrameJson {
    /// The name to show for this frame.
    #[must_use]
    pub fn name(&self) -> Option<&str> {
        self.hew_fn.as_deref().or(self.function.as_deref())
    }

    fn fill(&mut self, location: Location) {
        if self.hew_fn.is_none() && self.function.is_none() {
            self.function = location.function;
        }
        if self.file.is_none() {
            self.file = location.file;
            self.line = location.line;
            self.col = location.column;
        }
    }

    fn complete(&self) -> bool {
        self.name().is_some() && self.file.is_some()
    }
}

impl FailureRecordJson {
    /// Parse one line of a failure log.
    ///
    /// # Errors
    /// Fails on malformed JSON or a frame without a `pc`.
    pub fn parse(line: &str) -> serde_json::Result<Self> {
        serde_json::from_str(line)
    }

    /// The record as one log line, without its newline.
    ///
    /// # Errors
    /// Fails only if a kept unknown field cannot be written back.
    pub fn to_line(&self) -> serde_json::Result<String> {
        serde_json::to_string(self)
    }

    /// The frames in the form [`Symbolizer::symbolize`] takes. A frame whose
    /// address does not parse is skipped.
    #[must_use]
    pub fn frames(&self) -> Vec<(usize, Frame)> {
        let build = self.build.as_ref();
        let image_base = build
            .and_then(|build| build.image_base.as_deref())
            .and_then(parse_hex)
            .unwrap_or(0);
        let build_id = build
            .and_then(|build| build.build_id.as_deref())
            .and_then(BuildId::from_hex);
        let signalled = self.signal.is_some();
        self.frames
            .iter()
            .enumerate()
            .filter_map(|(index, frame)| {
                Some((
                    index,
                    Frame {
                        pc: parse_hex(&frame.pc)?,
                        image_base,
                        build_id: build_id.clone(),
                        return_address: !(index == 0 && signalled),
                    },
                ))
            })
            .collect()
    }
}

impl Symbolizer {
    /// Fill each frame's missing name and source position from these
    /// symbols, keeping what the runtime already resolved. Returns how many
    /// frames gained something.
    ///
    /// # Errors
    /// Refuses a record from a different build, and reports malformed debug
    /// information.
    pub fn symbolize_record(&self, record: &mut FailureRecordJson) -> Result<usize> {
        let frames = record.frames();
        let build_id = record
            .build
            .as_ref()
            .and_then(|build| build.build_id.as_deref())
            .and_then(BuildId::from_hex);
        crate::check_identity(self, build_id.as_ref())?;
        let mut filled = 0;
        for (index, frame) in frames {
            let slot = &mut record.frames[index];
            if slot.complete() {
                continue;
            }
            let Some(location) = self.symbolize(&frame)?.into_iter().next() else {
                continue;
            };
            let before = slot.clone();
            slot.fill(location);
            filled += usize::from(*slot != before);
        }
        Ok(filled)
    }
}

fn parse_hex(text: &str) -> Option<u64> {
    let digits = text
        .strip_prefix("0x")
        .or_else(|| text.strip_prefix("0X"))?;
    u64::from_str_radix(digits, 16).ok()
}
