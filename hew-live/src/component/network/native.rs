//! One owning thread for the generated native C presentation.

use std::ffi::c_void;
use std::path::PathBuf;
use std::ptr;
use std::sync::{mpsc, Arc};

use crate::native::{Generation, NativeHost};

use super::{Module, Receipt, Result};

pub(super) type Pointer = *mut c_void;
type ReadText = unsafe extern "C" fn(*const c_void, *mut usize) -> *const u8;
type Release = unsafe extern "C" fn(Pointer);
type NewText = unsafe extern "C" fn(*const u8, usize, *mut Pointer, *mut Pointer) -> i32;
type ReleaseValue = unsafe extern "C" fn(Pointer, *mut Pointer) -> i32;

#[derive(Clone, Copy)]
pub(super) struct Api {
    text_new: NewText,
    text_data: ReadText,
    text_release: Release,
    error_data: ReadText,
    error_release: Release,
    value_release: ReleaseValue,
    runtime_release: Release,
    pub sink_write: unsafe extern "C" fn(Pointer, *const c_void, usize),
    pub sink_close: Release,
}

impl Api {
    unsafe fn load(host: &NativeHost) -> Result<Self> {
        // SAFETY: these signatures are the canonical same-build host and Sink ABI.
        unsafe {
            Ok(Self {
                text_new: *host.runtime_symbol(b"hew_host_text_from_utf8")?,
                text_data: *host.runtime_symbol(b"hew_host_text_data")?,
                text_release: *host.runtime_symbol(b"hew_host_text_release")?,
                error_data: *host.runtime_symbol(b"hew_host_error_message")?,
                error_release: *host.runtime_symbol(b"hew_host_error_release")?,
                value_release: *host.runtime_symbol(b"hew_host_value_release")?,
                runtime_release: *host.runtime_symbol(b"hew_runtime_release")?,
                sink_write: *host.runtime_symbol(b"hew_sink_write")?,
                sink_close: *host.runtime_symbol(b"hew_sink_close")?,
            })
        }
    }

    fn check(self, status: i32, error: Pointer) -> Result<()> {
        if status == 0 {
            return Ok(());
        }
        let message = if error.is_null() {
            format!("native call failed with status {status}")
        } else {
            let mut length = 0;
            // SAFETY: a failed checked call returns one independent error owner.
            unsafe {
                let bytes = (self.error_data)(error, &raw mut length);
                let message =
                    String::from_utf8_lossy(std::slice::from_raw_parts(bytes, length)).into_owned();
                (self.error_release)(error);
                message
            }
        };
        Err(message.into())
    }

    pub(super) fn text(self, value: &str) -> Result<Text> {
        let mut pointer = ptr::null_mut();
        let mut error = ptr::null_mut();
        // SAFETY: borrowed UTF-8 and distinct empty output slots live through this call.
        let status = unsafe {
            (self.text_new)(
                value.as_ptr(),
                value.len(),
                &raw mut pointer,
                &raw mut error,
            )
        };
        self.check(status, error)?;
        Ok(Text { pointer, api: self })
    }

    pub(super) unsafe fn read(self, pointer: *const c_void) -> String {
        if pointer.is_null() {
            return String::new();
        }
        let mut length = 0;
        // SAFETY: callers retain an immutable Hew string throughout this synchronous borrow.
        unsafe {
            let bytes = (self.text_data)(pointer, &raw mut length);
            String::from_utf8_lossy(std::slice::from_raw_parts(bytes, length)).into_owned()
        }
    }

    fn release_value(self, pointer: Pointer) -> Result<()> {
        let mut error = ptr::null_mut();
        // SAFETY: this thread transfers its unique initialized opaque owner.
        let status = unsafe { (self.value_release)(pointer, &raw mut error) };
        self.check(status, error)
    }
}

pub(super) struct Text {
    pub pointer: Pointer,
    api: Api,
}

impl Drop for Text {
    fn drop(&mut self) {
        // SAFETY: this thread releases its independent text owner once.
        unsafe { (self.api.text_release)(self.pointer) };
    }
}

type Job = Box<dyn FnOnce(&mut Driver) + Send>;

#[derive(Clone)]
pub(super) struct Worker {
    commands: mpsc::Sender<Job>,
    pub api: Api,
}

impl Worker {
    pub async unsafe fn open(runtime: PathBuf, module: Module) -> Result<Self> {
        let (commands, receiver) = mpsc::channel::<Job>();
        let (ready, started) = tokio::sync::oneshot::channel();
        std::thread::Builder::new()
            .name("hew-node-host".into())
            .spawn(move || {
                // SAFETY: the caller establishes trusted immutable native artefacts.
                let mut driver = match unsafe { Driver::open(&runtime, &module) } {
                    Ok(driver) => driver,
                    Err(error) => {
                        let _ = ready.send(Err(error));
                        return;
                    }
                };
                if ready.send(Ok(driver.api)).is_err() {
                    return;
                }
                while let Ok(job) = receiver.recv() {
                    job(&mut driver);
                }
            })?;
        let api = started
            .await
            .map_err(|_| "native node worker closed during startup")??;
        Ok(Self { commands, api })
    }

    pub async fn execute<T: Send + 'static>(
        &self,
        operation: impl FnOnce(&mut Driver) -> Result<T> + Send + 'static,
    ) -> Result<T> {
        let (reply, result) = tokio::sync::oneshot::channel();
        self.submit(move |driver| {
            let _ = reply.send(operation(driver));
        })?;
        result.await.map_err(|_| "native node worker closed")?
    }

    pub fn submit(&self, operation: impl FnOnce(&mut Driver) + Send + 'static) -> Result<()> {
        self.commands
            .send(Box::new(operation))
            .map_err(|_| "native node worker closed".into())
    }
}

pub(super) struct Driver {
    pub api: Api,
    generation: Generation,
    runtime: Pointer,
    server: Pointer,
    client: Pointer,
    grants: Vec<Pointer>,
    pub context: Option<Arc<super::control::Control>>,
}

impl Driver {
    unsafe fn open(runtime: &std::path::Path, module: &Module) -> Result<Self> {
        // SAFETY: caller promises trusted, immutable same-build runtime/module files.
        let host = unsafe {
            NativeHost::open(runtime, &module.manifest.compiler, &module.manifest.target)?
        };
        // SAFETY: the module has the preparation receipt for that runtime.
        let generation = unsafe { host.load(&module.path, &module.manifest)? };
        // SAFETY: canonical host signatures are fixed by the same build.
        let api = unsafe { Api::load(&host)? };
        // SAFETY: this thread becomes the runtime's one external lifecycle owner.
        let runtime = unsafe {
            host.runtime_symbol::<unsafe extern "C" fn() -> Pointer>(b"hew_runtime_new")?()
        };
        Ok(Self {
            api,
            generation,
            runtime,
            server: ptr::null_mut(),
            client: ptr::null_mut(),
            grants: Vec::new(),
            context: None,
        })
    }

    pub unsafe fn symbol<T: Copy>(&self, name: &[u8]) -> Result<T> {
        // SAFETY: callers match the SDK-generated node header and this driver pins its code.
        Ok(unsafe { *self.generation.symbol::<T>(name)? })
    }

    pub fn start(&mut self) -> Result<()> {
        type Start = unsafe extern "C" fn(*mut Pointer, *mut Pointer) -> i32;
        let mut error = ptr::null_mut();
        // SAFETY: network_start returns one Server owner into an empty slot.
        let status = unsafe {
            self.symbol::<Start>(b"network_start")?(&raw mut self.server, &raw mut error)
        };
        self.api.check(status, error)
    }

    pub fn grant(&mut self, identity: &str) -> Result<()> {
        type Grant =
            unsafe extern "C" fn(*const c_void, *const c_void, *mut Pointer, *mut Pointer) -> i32;
        let identity = self.api.text(identity)?;
        let mut value = ptr::null_mut();
        let mut error = ptr::null_mut();
        // SAFETY: the live Server and text remain borrowed until this synchronous call finishes.
        let status = unsafe {
            self.symbol::<Grant>(b"network_grant")?(
                self.server,
                identity.pointer,
                &raw mut value,
                &raw mut error,
            )
        };
        self.api.check(status, error)?;
        self.grants.push(value);
        Ok(())
    }

    pub fn connect(&mut self, endpoint: &str, identity: &str) -> Result<()> {
        type Connect =
            unsafe extern "C" fn(*const c_void, *const c_void, *mut Pointer, *mut Pointer) -> i32;
        let endpoint = self.api.text(endpoint)?;
        let identity = self.api.text(identity)?;
        let mut error = ptr::null_mut();
        // SAFETY: independent texts and an empty Client slot follow the generated signature.
        let status = unsafe {
            self.symbol::<Connect>(b"network_connect")?(
                endpoint.pointer,
                identity.pointer,
                &raw mut self.client,
                &raw mut error,
            )
        };
        self.api.check(status, error)
    }

    pub fn prepare(&mut self, path: &str) -> Result<String> {
        type Prepare =
            unsafe extern "C" fn(*const c_void, *const c_void, *mut Pointer, *mut Pointer) -> i32;
        let path = self.api.text(path)?;
        let mut value = ptr::null_mut();
        let mut error = ptr::null_mut();
        // SAFETY: Client/text borrows and a fresh text output match the generated header.
        let status = unsafe {
            self.symbol::<Prepare>(b"network_client_prepare")?(
                self.client,
                path.pointer,
                &raw mut value,
                &raw mut error,
            )
        };
        self.api.check(status, error)?;
        let value = Text {
            pointer: value,
            api: self.api,
        };
        // SAFETY: the returned text stays owned until its contents are copied.
        Ok(unsafe { self.api.read(value.pointer) })
    }

    pub fn status(&mut self) -> Result<super::super::host::Status> {
        type Status = unsafe extern "C" fn(*const c_void, *mut Pointer, *mut Pointer) -> i32;
        let mut value = ptr::null_mut();
        let mut error = ptr::null_mut();
        // SAFETY: Client and empty Status/error output slots match the generated header.
        let status = unsafe {
            self.symbol::<Status>(b"network_client_status")?(
                self.client,
                &raw mut value,
                &raw mut error,
            )
        };
        self.api.check(status, error)?;
        let result = self.read_status(value, false);
        let released = self.api.release_value(value);
        let status = result?;
        released?;
        Ok(status)
    }

    pub fn replace(&mut self, digest: &str, predecessor: u64, within_ms: u64) -> Result<Receipt> {
        type Replace = unsafe extern "C" fn(
            *const c_void,
            *const c_void,
            u64,
            u64,
            *mut Pointer,
            *mut Pointer,
        ) -> i32;
        let digest = self.api.text(digest)?;
        let mut value = ptr::null_mut();
        let mut error = ptr::null_mut();
        // SAFETY: all inputs and fresh Receipt/error slots follow the generated signature.
        let status = unsafe {
            self.symbol::<Replace>(b"network_client_replace")?(
                self.client,
                digest.pointer,
                predecessor,
                within_ms,
                &raw mut value,
                &raw mut error,
            )
        };
        self.api.check(status, error)?;
        let result: Result<Receipt> = (|| {
            Ok(Receipt {
                status: self.read_status(value, true)?,
                accepted: self.number(value, b"network_receipt_accepted")?,
            })
        })();
        let released = self.api.release_value(value);
        let receipt = result?;
        released?;
        Ok(receipt)
    }

    pub fn route(&mut self, source: &str, body: &str) -> Result<bool> {
        type Route = unsafe extern "C" fn(
            *const c_void,
            *const c_void,
            *const c_void,
            *mut u8,
            *mut Pointer,
        ) -> i32;
        let source = self.api.text(source)?;
        let body = self.api.text(body)?;
        let mut value = 0;
        let mut error = ptr::null_mut();
        // SAFETY: Client/text borrows and scalar/error slots match the generated header.
        let status = unsafe {
            self.symbol::<Route>(b"network_client_route")?(
                self.client,
                source.pointer,
                body.pointer,
                &raw mut value,
                &raw mut error,
            )
        };
        self.api.check(status, error)?;
        Ok(value != 0)
    }

    fn number(&self, owner: Pointer, name: &[u8]) -> Result<u64> {
        type Number = unsafe extern "C" fn(*const c_void, *mut u64, *mut Pointer) -> i32;
        let mut value = 0;
        let mut error = ptr::null_mut();
        // SAFETY: callers select the getter for this retained opaque owner.
        let status = unsafe { self.symbol::<Number>(name)?(owner, &raw mut value, &raw mut error) };
        self.api.check(status, error)?;
        Ok(value)
    }

    fn read_status(&self, owner: Pointer, receipt: bool) -> Result<super::super::host::Status> {
        type Digest = unsafe extern "C" fn(*const c_void, *mut Pointer, *mut Pointer) -> i32;
        let epoch = self.number(
            owner,
            if receipt {
                b"network_receipt_epoch"
            } else {
                b"network_status_epoch"
            },
        )?;
        let mut value = ptr::null_mut();
        let mut error = ptr::null_mut();
        // SAFETY: the selected getter borrows the matching owner and returns independent text.
        let status = unsafe {
            self.symbol::<Digest>(if receipt {
                b"network_receipt_digest"
            } else {
                b"network_status_digest"
            })?(owner, &raw mut value, &raw mut error)
        };
        self.api.check(status, error)?;
        let value = Text {
            pointer: value,
            api: self.api,
        };
        // SAFETY: value remains owned until the copy is complete.
        let digest = super::parse_digest(&unsafe { self.api.read(value.pointer) })?;
        Ok(super::super::host::Status { epoch, digest })
    }

    pub fn close(&mut self) -> Result<()> {
        let mut failure = None;
        for value in self.grants.drain(..).chain([
            std::mem::replace(&mut self.server, ptr::null_mut()),
            std::mem::replace(&mut self.client, ptr::null_mut()),
        ]) {
            if value.is_null() {
                continue;
            }
            if let Err(error) = self.api.release_value(value) {
                failure.get_or_insert(error);
            }
        }
        if self.context.is_some() {
            type Uninstall = unsafe extern "C" fn();
            // SAFETY: every server actor and callback has drained before clearing its table.
            match unsafe { self.symbol::<Uninstall>(b"live_control_uninstall") } {
                Ok(uninstall) => {
                    // SAFETY: the owning module is pinned and has no live callbacks.
                    unsafe {
                        uninstall();
                    }
                }
                Err(error) => {
                    failure.get_or_insert(error);
                }
            }
        }
        self.context = None;
        if !self.runtime.is_null() {
            // SAFETY: all hosted values and synchronous calls have completed on this thread.
            unsafe {
                (self.api.runtime_release)(std::mem::replace(&mut self.runtime, ptr::null_mut()));
            };
        }
        failure.map_or(Ok(()), Err)
    }
}

impl Drop for Driver {
    fn drop(&mut self) {
        if let Err(error) = self.close() {
            eprintln!("hew-live-node: native cleanup failed: {error}");
        }
    }
}
