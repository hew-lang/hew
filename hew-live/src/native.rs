//! Loadable modules importing a single runtime, with reachability retirement.

use std::collections::{BTreeMap, BTreeSet};
use std::fmt;
use std::path::Path;
use std::process::Command;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex};

use libloading::{Library, Symbol};
use object::{Object, ObjectSymbol};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

static RUNTIME_LOADED: AtomicBool = AtomicBool::new(false);

/// Exact prepared code and runtime identity. Version labels alone are insufficient.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Manifest {
    pub compiler: String,
    pub target: String,
    pub runtime_sha256: String,
    pub module_sha256: String,
}

fn digest(bytes: &[u8]) -> String {
    use std::fmt::Write;
    Sha256::digest(bytes)
        .iter()
        .fold(String::with_capacity(64), |mut hex, byte| {
            write!(hex, "{byte:02x}").expect("writing to a String cannot fail");
            hex
        })
}

/// Link a verified Hew host-export object against the supplied shared runtime.
///
/// The output is private preparation: no module code is executed. The caller
/// publishes the returned manifest only after this function succeeds. Native
/// dependencies must already be incorporated into the host's shared runtime.
///
/// # Errors
/// Returns object-validation, linker or filesystem errors. Currently the linker
/// driver supports ELF and Mach-O hosts; other hosts receive an explicit error.
pub fn prepare(
    object_path: &Path,
    runtime: &Path,
    output: &Path,
    compiler: &str,
    target: &str,
) -> Result<Manifest> {
    let object_bytes = std::fs::read(object_path)?;
    reject_runtime_definitions(&object_bytes)?;
    let runtime = runtime.canonicalize()?;
    let runtime_bytes = std::fs::read(&runtime)?;
    let format = object::File::parse(runtime_bytes.as_slice())?.format();
    let parent = output
        .parent()
        .filter(|path| !path.as_os_str().is_empty())
        .unwrap_or(Path::new("."));
    let prepared = tempfile::NamedTempFile::new_in(parent)?;
    let mut link = Command::new("cc");
    match format {
        object::BinaryFormat::Elf => {
            link.args(["-shared", "-Wl,-z,defs"]);
        }
        object::BinaryFormat::MachO => {
            link.arg("-dynamiclib");
        }
        _ => {
            return Err("native generation linking is not implemented for this host format".into())
        }
    }
    link.arg(object_path)
        .arg(&runtime)
        .arg("-o")
        .arg(prepared.path());
    let linked = link.output()?;
    if !linked.status.success() {
        return Err(format!(
            "native generation link failed: {}",
            String::from_utf8_lossy(&linked.stderr)
        )
        .into());
    }
    let module = std::fs::read(prepared.path())?;
    reject_runtime_definitions(&module)?;
    // Never overwrite a generation another invocation may already be executing.
    prepared.persist_noclobber(output)?;
    Ok(Manifest {
        compiler: compiler.into(),
        target: target.into(),
        runtime_sha256: digest(&runtime_bytes),
        module_sha256: digest(&module),
    })
}

fn reject_runtime_definitions(bytes: &[u8]) -> Result<()> {
    let file = object::File::parse(bytes)?;
    for symbol in file.symbols().chain(file.dynamic_symbols()) {
        if symbol.is_definition() && symbol.is_global() {
            let name = symbol.name()?.trim_start_matches('_');
            if name.starts_with("hew_") {
                return Err(format!(
                    "module defines runtime symbol `{name}`; import the host runtime instead"
                )
                .into());
            }
        }
    }
    Ok(())
}

struct Runtime {
    // The library stays loaded until every generation and borrowed symbol dies.
    library: Library,
    sha256: String,
    compiler: String,
    target: String,
}

/// A host sharing one runtime image across every native generation.
#[derive(Clone)]
pub struct NativeHost {
    runtime: Arc<Runtime>,
    generations: Arc<Mutex<Generations>>,
}

impl fmt::Debug for NativeHost {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeHost")
            .field("runtime_sha256", &self.runtime.sha256)
            .finish_non_exhaustive()
    }
}

struct Loaded {
    library: Library,
    _runtime: Arc<Runtime>,
}

#[derive(Default)]
struct Generations {
    next: u64,
    nodes: BTreeMap<u64, GenerationNode>,
}

struct GenerationNode {
    loaded: Arc<Loaded>,
    dependencies: BTreeSet<u64>,
}

/// An executable root. Keep one with every value, frame, actor, callback,
/// destructor descriptor or borrowed foreign view that may enter this code.
/// Dependency edges themselves do not root a generation, so cycles can retire.
#[derive(Clone)]
pub struct Generation {
    id: u64,
    loaded: Arc<Loaded>,
    host: NativeHost,
}

impl fmt::Debug for Generation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Generation")
            .field("id", &self.id)
            .finish_non_exhaustive()
    }
}

impl NativeHost {
    /// Open the process's sole native runtime image.
    ///
    /// # Safety
    /// The runtime must be a trusted build compatible with `compiler` and
    /// `target`. The process must not already link another Hew runtime. Its
    /// directory and loaded artefacts must remain immutable while in use.
    /// Loading native code executes platform initialization routines.
    ///
    /// # Errors
    /// Returns I/O, loader errors, or refusal of a second runtime image.
    pub unsafe fn open(runtime: &Path, compiler: &str, target: &str) -> Result<Self> {
        let runtime = runtime.canonicalize()?;
        let sha256 = digest(&std::fs::read(&runtime)?);
        RUNTIME_LOADED
            .compare_exchange(false, true, Ordering::AcqRel, Ordering::Acquire)
            .map_err(|_| "a native runtime is already loaded; clone the existing host")?;
        // SAFETY: the caller guarantees the trusted, immutable runtime boundary.
        let library = match unsafe { Library::new(&runtime) } {
            Ok(library) => library,
            Err(error) => {
                RUNTIME_LOADED.store(false, Ordering::Release);
                return Err(error.into());
            }
        };
        Ok(Self {
            runtime: Arc::new(Runtime {
                library,
                sha256,
                compiler: compiler.into(),
                target: target.into(),
            }),
            generations: Arc::new(Mutex::new(Generations::default())),
        })
    }

    /// Borrow an entry point from the one runtime.
    ///
    /// # Safety
    /// `T` must match the symbol's ABI. Callers must obey its ownership contract
    /// and finish runtime work before releasing its code roots.
    /// # Errors
    /// Returns a loader error when the symbol does not exist.
    pub unsafe fn runtime_symbol<T>(&self, name: &[u8]) -> Result<Symbol<'_, T>> {
        // SAFETY: the caller supplies the symbol's exact ABI.
        Ok(unsafe { self.runtime.library.get(name)? })
    }

    /// Admit prepared code without running any exported entry point.
    ///
    /// # Safety
    /// The manifest and artefact must come from the trusted preparation path,
    /// remain immutable, and contain no initialization or finalization routines
    /// that perform Hew effects. A hash is integrity evidence, not provenance.
    /// Every executable reference escaping a call must retain a generation root.
    ///
    /// # Errors
    /// Rejects incompatible code before opening it, runtime definitions, changed
    /// code, and platform loader failures.
    pub unsafe fn load(&self, path: &Path, manifest: &Manifest) -> Result<Generation> {
        type RuntimeNew = unsafe extern "C" fn() -> *mut std::ffi::c_void;
        if manifest.compiler != self.runtime.compiler
            || manifest.target != self.runtime.target
            || manifest.runtime_sha256 != self.runtime.sha256
        {
            return Err("native generation compiler, target or runtime identity mismatch".into());
        }
        let bytes = std::fs::read(path)?;
        if digest(&bytes) != manifest.module_sha256 {
            return Err("native generation changed after preparation".into());
        }
        reject_runtime_definitions(&bytes)?;
        // SAFETY: trusted preparation and immutable artefacts are caller obligations.
        let library = unsafe { Library::new(path)? };
        // SAFETY: these are the runtime's fixed lifecycle entry points. Only
        // their addresses are compared; neither is called during preparation.
        let same_runtime = unsafe {
            let host = self.runtime.library.get::<RuntimeNew>(b"hew_runtime_new")?;
            let imported = library.get::<RuntimeNew>(b"hew_runtime_new")?;
            std::ptr::fn_addr_eq(*host, *imported)
        };
        if !same_runtime {
            return Err("native generation resolves a different runtime image".into());
        }
        let loaded = Arc::new(Loaded {
            library,
            _runtime: self.runtime.clone(),
        });
        let mut generations = self
            .generations
            .lock()
            .map_err(|_| "generation registry poisoned")?;
        let id = generations.next;
        generations.next = id.checked_add(1).ok_or("generation identity exhausted")?;
        generations.nodes.insert(
            id,
            GenerationNode {
                loaded: loaded.clone(),
                dependencies: BTreeSet::new(),
            },
        );
        Ok(Generation {
            id,
            loaded,
            host: self.clone(),
        })
    }

    /// Retire all generations unreachable from executable roots, including cycles.
    ///
    /// # Errors
    /// Returns an error if the registry was poisoned by a panic.
    pub fn collect(&self) -> Result<Vec<u64>> {
        let mut generations = self
            .generations
            .lock()
            .map_err(|_| "generation registry poisoned")?;
        let mut pending: Vec<_> = generations
            .nodes
            .iter()
            .filter(|(_, node)| Arc::strong_count(&node.loaded) > 1)
            .map(|(&id, _)| id)
            .collect();
        let mut reachable = BTreeSet::new();
        while let Some(id) = pending.pop() {
            if reachable.insert(id) {
                if let Some(node) = generations.nodes.get(&id) {
                    pending.extend(&node.dependencies);
                }
            }
        }
        let retired: Vec<_> = generations
            .nodes
            .keys()
            .filter(|id| !reachable.contains(id))
            .copied()
            .collect();
        let mut libraries = Vec::new();
        for id in &retired {
            libraries.push(generations.nodes.remove(id));
        }
        // Platform finalizers may call back into the host. Never hold its lock.
        drop(generations);
        drop(libraries);
        Ok(retired)
    }
}

impl Generation {
    /// Register code reachable from this generation. Registration requires live
    /// roots at both ends and therefore cannot resurrect retired code.
    /// # Errors
    /// Refuses an edge between different runtime hosts or a poisoned registry.
    pub fn depends_on(&self, dependency: &Self) -> Result<()> {
        if !Arc::ptr_eq(&self.host.generations, &dependency.host.generations) {
            return Err("native generation dependency belongs to another host".into());
        }
        let mut generations = self
            .host
            .generations
            .lock()
            .map_err(|_| "generation registry poisoned")?;
        generations
            .nodes
            .get_mut(&self.id)
            .ok_or("native generation has retired")?
            .dependencies
            .insert(dependency.id);
        Ok(())
    }

    /// Borrow an export while this root keeps its code and dependencies loaded.
    /// # Safety
    /// `T` must match the generated export ABI. Every escaping executable value
    /// must carry a clone of this root, including callbacks held by host services.
    /// # Errors
    /// Returns the loader's missing-symbol or incompatible-symbol error.
    pub unsafe fn symbol<T>(&self, name: &[u8]) -> Result<Symbol<'_, T>> {
        // SAFETY: the caller supplies the checked generated export signature.
        Ok(unsafe { self.loaded.library.get(name)? })
    }
}
