//! One persistent execution process, separate from the compiler's static runtime.

use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::ffi::CString;
use std::io::{BufRead, BufReader, Write};
use std::net::{SocketAddr, TcpStream};
use std::path::PathBuf;

use clap::Parser;
use hew_live::{
    native::{Manifest, NativeHost},
    session::NativeSession,
};
use serde::Deserialize;
use serde_json::{json, Value};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

#[derive(Debug, Parser)]
#[command(about = "Experimental same-build Hew live execution worker")]
struct Args {
    #[arg(long)]
    runtime: PathBuf,
    #[arg(long)]
    compiler: String,
    #[arg(long)]
    target: String,
    /// The local parent owns this control listener. Program I/O uses stdio.
    #[arg(long)]
    control: SocketAddr,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "op", rename_all = "snake_case", deny_unknown_fields)]
enum Request {
    Prepare {
        id: u64,
        module: PathBuf,
        manifest: Manifest,
    },
    Execute {
        id: u64,
        entry: String,
        slot_count: usize,
    },
    Inspect,
    Release {
        slots: Vec<usize>,
    },
    Clear,
    Retire {
        id: u64,
    },
    Collect,
    Quit,
}

fn run(args: &Args) -> Result<()> {
    if !args.control.ip().is_loopback() {
        return Err("live control must connect to a local parent".into());
    }
    // Keep ordinary Hew stdout/stderr separate from ordered control replies.
    // Commands are never retried by this worker.
    let mut control = TcpStream::connect(args.control)?;
    control.set_nodelay(true)?;
    let reader = BufReader::new(control.try_clone()?);
    // SAFETY: the trusted local worker links no static runtime and is launched
    // by the same-build compiler with private prepared artefacts.
    let host = unsafe { NativeHost::open(&args.runtime, &args.compiler, &args.target)? };
    let mut session = NativeSession::new(host.clone())?;
    let mut generations = BTreeMap::new();
    for line in reader.lines() {
        let request = serde_json::from_str::<Request>(&line?);
        let quit = matches!(request, Ok(Request::Quit));
        let reply: Result<Value> = (|| {
            Ok(match request? {
                Request::Prepare {
                    id,
                    module,
                    manifest,
                } => {
                    let Entry::Vacant(entry) = generations.entry(id) else {
                        return Err(format!("generation {id} is already prepared").into());
                    };
                    // SAFETY: only the trusted compiler controls this channel;
                    // native loading is not an isolation boundary.
                    entry.insert(unsafe { host.load(&module, &manifest)? });
                    json!({ "prepared": id })
                }
                Request::Execute {
                    id,
                    entry,
                    slot_count,
                } => {
                    let generation = generations.get(&id).ok_or("unknown generation")?;
                    let entry = CString::new(entry)?;
                    // SAFETY: the compiler supplies the exact session ABI,
                    // retained types/slots and executable-provenance tracking.
                    let execution = unsafe { session.execute(generation, &entry, slot_count)? };
                    json!({ "execution": execution })
                }
                Request::Inspect => json!({ "slots": session.states() }),
                Request::Release { slots } => json!({
                    "cleanup_faults": session.release(&slots)?,
                    "slots": session.states(),
                }),
                Request::Clear | Request::Quit => json!({ "cleanup_faults": session.clear() }),
                Request::Retire { id } => {
                    generations.remove(&id).ok_or("unknown generation")?;
                    json!({ "collected": host.collect()? })
                }
                Request::Collect => json!({ "collected": host.collect()? }),
            })
        })();
        let reply = reply.unwrap_or_else(|error| json!({ "error": error.to_string() }));
        serde_json::to_writer(&mut control, &reply)?;
        control.write_all(b"\n")?;
        control.flush()?;
        if quit {
            break;
        }
    }
    // A disconnect ends this worker; uncertain work is never replayed. Explicit
    // Quit/Clear return cleanup errors while the parent remains connected.
    let _ = session.clear();
    generations.clear();
    host.collect()?;
    Ok(())
}

fn main() {
    if let Err(error) = run(&Args::parse()) {
        eprintln!("hew-live-host: {error}");
        std::process::exit(1);
    }
}
