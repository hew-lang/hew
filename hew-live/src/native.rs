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
            // Even an empty declaration submission belongs to this runtime.
            // Keep that dependency when the object needs no runtime symbols.
            link.args(["-shared", "-Wl,-z,defs", "-Wl,--no-as-needed"]);
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
    module_sha256: String,
    library: Library,
    _runtime: Arc<Runtime>,
    lease: hew_cabi::code::HewCodeLease,
    retiring: Mutex<bool>,
    finalize: Option<unsafe extern "C" fn()>,
    finalized: AtomicBool,
}

impl Loaded {
    fn finalize(&self) {
        if !self.finalized.swap(true, Ordering::AcqRel) {
            if let Some(finalize) = self.finalize {
                // SAFETY: the host retains this image through explicit unregister.
                unsafe { finalize() };
            }
        }
    }
}

unsafe extern "C" fn try_retain_code(context: *const std::ffi::c_void) -> bool {
    // SAFETY: registry protection retains the host node until unregister returns.
    let loaded = unsafe { &*context.cast::<Loaded>() };
    let Ok(retiring) = loaded.retiring.lock() else {
        return false;
    };
    if *retiring {
        return false;
    }
    // SAFETY: the host's registry node is retained under this admission gate.
    unsafe { Arc::increment_strong_count(context.cast::<Loaded>()) };
    true
}

unsafe extern "C" fn retain_code(context: *const std::ffi::c_void) {
    // SAFETY: every acquisition begins while a caller root retains this node.
    unsafe { Arc::increment_strong_count(context.cast::<Loaded>()) };
}

unsafe extern "C" fn release_code(context: *const std::ffi::c_void) {
    // SAFETY: the runtime transfers exactly one executable root to this call.
    unsafe { Arc::decrement_strong_count(context.cast::<Loaded>()) };
}

#[derive(Default)]
struct Generations {
    next: u64,
    nodes: BTreeMap<u64, GenerationNode>,
}

impl Drop for Generations {
    fn drop(&mut self) {
        for node in self.nodes.values() {
            *node
                .loaded
                .retiring
                .lock()
                .unwrap_or_else(std::sync::PoisonError::into_inner) = true;
            node.loaded.finalize();
        }
    }
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
    pub(crate) fn owns(&self, generation: &Generation) -> bool {
        Arc::ptr_eq(&self.runtime, &generation.host.runtime)
    }

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
        type ModuleHook = unsafe extern "C" fn();
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
        let mut generations = self
            .generations
            .lock()
            .map_err(|_| "generation registry poisoned")?;
        if let Some((&id, node)) = generations
            .nodes
            .iter()
            .find(|(_, node)| node.loaded.module_sha256 == manifest.module_sha256)
        {
            return Ok(Generation {
                id,
                loaded: node.loaded.clone(),
                host: self.clone(),
            });
        }
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
        // A platform loader may reuse an image even through a different path.
        // Its published owner is authoritative and must never be rebound.
        // SAFETY: the optional export is the compiler's fixed writable slot ABI.
        let code_owner =
            unsafe { library.get::<*mut hew_cabi::code::HewCodeLeaseSlot>(b"HewLiveCodeOwner") }
                .ok()
                .map(|symbol| *symbol);
        if let Some(slot) = code_owner {
            // SAFETY: trusted compiler metadata remains valid with the library.
            let published = unsafe { (*slot).lease };
            if !published.is_null() {
                let (&id, node) = generations
                    .nodes
                    .iter()
                    .find(|(_, node)| std::ptr::eq(&raw const node.loaded.lease, published))
                    .ok_or("native image has an unknown defining-code owner")?;
                if node.loaded.module_sha256 != manifest.module_sha256 {
                    return Err("native loader reused an image with different prepared code".into());
                }
                return Ok(Generation {
                    id,
                    loaded: node.loaded.clone(),
                    host: self.clone(),
                });
            }
        }
        // SAFETY: these optional exports are the compiler's same-build metadata hooks.
        let initialize = unsafe { library.get::<ModuleHook>(b"HewLiveInitialize") }
            .ok()
            .map(|symbol| *symbol);
        // SAFETY: same explicit metadata hook ABI, invoked while the image is retained.
        let finalize = unsafe { library.get::<ModuleHook>(b"HewLiveFinalize") }
            .ok()
            .map(|symbol| *symbol);
        let id = generations.next;
        generations.next = id.checked_add(1).ok_or("generation identity exhausted")?;
        let loaded = Arc::new_cyclic(|owner: &std::sync::Weak<Loaded>| Loaded {
            module_sha256: manifest.module_sha256.clone(),
            library,
            _runtime: self.runtime.clone(),
            lease: hew_cabi::code::HewCodeLease {
                context: owner.as_ptr().cast(),
                retain: retain_code,
                release: release_code,
                try_retain: Some(try_retain_code),
            },
            retiring: Mutex::new(false),
            finalize,
            finalized: AtomicBool::new(false),
        });
        if let Some(slot) = code_owner {
            // SAFETY: admission is serialized and this new image is unpublished.
            unsafe { (*slot).lease = &raw const loaded.lease };
        }
        generations.nodes.insert(
            id,
            GenerationNode {
                loaded: loaded.clone(),
                dependencies: BTreeSet::new(),
            },
        );
        if let Some(initialize) = initialize {
            // SAFETY: lease publication and graph ownership precede registration.
            unsafe { initialize() };
        }
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
        // Freeze registry-based acquisitions while deciding reachability. Normal
        // retain already has a caller root and cannot resurrect an unrooted node.
        let mut admissions = generations
            .nodes
            .iter()
            .map(|(&id, node)| {
                node.loaded
                    .retiring
                    .lock()
                    .map(|gate| (id, gate))
                    .map_err(|_| "generation admission poisoned")
            })
            .collect::<std::result::Result<Vec<_>, _>>()?;
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
        for (id, gate) in &mut admissions {
            if !reachable.contains(id) {
                **gate = true;
            }
        }
        drop(admissions);
        let mut libraries = Vec::new();
        for id in &retired {
            libraries.push(generations.nodes.remove(id));
        }
        // Platform finalizers may call back into the host. Never hold its lock.
        drop(generations);
        for node in libraries.iter().flatten() {
            node.loaded.finalize();
        }
        drop(libraries);
        Ok(retired)
    }
}

impl Generation {
    /// Host-local identity returned by this host's retirement receipts.
    #[must_use]
    pub const fn id(&self) -> u64 {
        self.id
    }

    /// The defining-code lease used by this generation's typed descriptors.
    /// The returned record stays valid while this handle or an acquired lease
    /// root lives. Descriptor publication must precede executing module code.
    #[must_use]
    pub fn code_lease(&self) -> &hew_cabi::code::HewCodeLease {
        &self.loaded.lease
    }

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

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use std::time::{Duration, Instant};

    static REGISTRY: Mutex<()> = Mutex::new(());
    static FINALIZING: AtomicBool = AtomicBool::new(false);

    unsafe extern "C" fn unregister() {
        FINALIZING.store(true, Ordering::Release);
        // Match the runtime registry's protection: removal waits for an
        // in-progress lookup, with no generated code called under this lock.
        drop(REGISTRY.lock().unwrap());
    }

    #[test]
    fn codec_lookup_and_retirement_have_one_admission_order() {
        let runtime = Arc::new(Runtime {
            library: libloading::os::unix::Library::this().into(),
            sha256: String::new(),
            compiler: String::new(),
            target: String::new(),
        });
        let host = NativeHost {
            runtime: runtime.clone(),
            generations: Arc::new(Mutex::new(Generations::default())),
        };
        let loaded = Arc::new_cyclic(|owner: &std::sync::Weak<Loaded>| Loaded {
            module_sha256: String::new(),
            library: libloading::os::unix::Library::this().into(),
            _runtime: runtime,
            lease: hew_cabi::code::HewCodeLease {
                context: owner.as_ptr().cast(),
                retain: retain_code,
                release: release_code,
                try_retain: Some(try_retain_code),
            },
            retiring: Mutex::new(false),
            finalize: Some(unregister),
            finalized: AtomicBool::new(false),
        });
        let context = loaded.lease.context;
        host.generations.lock().unwrap().nodes.insert(
            0,
            GenerationNode {
                loaded,
                dependencies: BTreeSet::new(),
            },
        );
        // A lookup admitted first supplies a real Arc root, so collection must
        // leave its callback usable even after the lookup releases its lock.
        // SAFETY: the registry node owns this lease record throughout admission.
        assert!(unsafe { try_retain_code(context) });
        assert!(host.collect().unwrap().is_empty());
        // SAFETY: release exactly the root admitted above.
        unsafe { release_code(context) };

        let protected_lookup = REGISTRY.lock().unwrap();
        let retirement = std::thread::spawn(move || host.collect().unwrap());
        let deadline = Instant::now() + Duration::from_secs(5);
        while !FINALIZING.load(Ordering::Acquire) {
            assert!(
                Instant::now() < deadline,
                "retirement did not reach unregister"
            );
            std::thread::yield_now();
        }
        // Collection has closed admission, but unregister must wait for this
        // lookup. The node still exists and refuses resurrection before unload.
        // SAFETY: the held registry lock keeps unregister and image release pending.
        assert!(!unsafe { try_retain_code(context) });
        drop(protected_lookup);
        assert_eq!(retirement.join().unwrap(), vec![0]);
    }
}
