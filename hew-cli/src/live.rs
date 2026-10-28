//! Experimental source authoring against one persistent native worker.

use std::collections::BTreeMap;
use std::io::{BufRead, BufReader, IsTerminal, Write};
use std::net::{Ipv4Addr, TcpListener, TcpStream};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, Stdio};
use std::time::Duration;

use hew_codegen_rs::physical::{emit_host_object, HostExport, PhysicalEmitOptions};
use hew_compile::live::frontend::{self, InputKind, LiveFrontend, PreparedInput};
use hew_live::session::{Execution, Fault};
use hew_types::session::SessionContents;
use serde::de::DeserializeOwned;
use serde_json::{json, Value};

use crate::eval::classify::{self, InputCompleteness, ReplCommand};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

struct RunningChild(Child);

impl Drop for RunningChild {
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

struct Worker {
    connection: BufReader<TcpStream>,
    _child: RunningChild,
}

impl Worker {
    fn start(executable: &Path, runtime: &Path, target: &str, timeout: Duration) -> Result<Self> {
        let listener = TcpListener::bind((Ipv4Addr::LOCALHOST, 0))?;
        let address = listener.local_addr()?;
        let mut child = RunningChild(
            Command::new(executable)
                .arg("--runtime")
                .arg(runtime)
                .args(["--compiler", hew_compile::VERSION, "--target", target])
                .arg("--control")
                .arg(address.to_string())
                .stdin(Stdio::null())
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .spawn()?,
        );
        let (send, receive) = std::sync::mpsc::sync_channel(1);
        let accepted = std::thread::Builder::new()
            .name("hew-live-connect".into())
            .spawn(move || {
                let _ = send.send(listener.accept());
            })?;
        let connected = receive.recv_timeout(timeout);
        if connected.is_err() {
            // End the one native wait before returning; no polling or retry.
            let _ = child.0.kill();
            let _ = TcpStream::connect(address);
        }
        accepted.join().map_err(|_| "live connection wait failed")?;
        let (connection, _) = connected.map_err(|_| "live worker did not connect")??;
        connection.set_nodelay(true)?;
        connection.set_read_timeout(Some(timeout))?;
        connection.set_write_timeout(Some(timeout))?;
        Ok(Self {
            connection: BufReader::new(connection),
            _child: child,
        })
    }

    fn exchange(&mut self, request: &Value) -> Result<Value> {
        serde_json::to_writer(self.connection.get_mut(), request)?;
        self.connection.get_mut().write_all(b"\n")?;
        self.connection.get_mut().flush()?;
        let mut line = String::new();
        let read = self.connection.read_line(&mut line).map_err(|error| {
            format!("live worker reply lost; execution outcome is uncertain: {error}")
        })?;
        if read == 0 {
            return Err("live worker disconnected; execution outcome is uncertain".into());
        }
        let reply: Value = serde_json::from_str(&line)?;
        if let Some(error) = reply.get("error").and_then(Value::as_str) {
            return Err(error.to_owned().into());
        }
        Ok(reply)
    }

    fn request<T: DeserializeOwned>(&mut self, request: &Value, field: &str) -> Result<T> {
        Ok(serde_json::from_value(
            self.exchange(request)?[field].take(),
        )?)
    }
}

struct Authoring {
    frontend: LiveFrontend,
    compiler: hew_compile::Session,
    worker: Worker,
    artefacts: tempfile::TempDir,
    runtime: PathBuf,
    project: PathBuf,
    target: String,
    opt_level: hew_codegen_rs::OptLevel,
    next_generation: u64,
    modules: BTreeMap<u64, PathBuf>,
    show_wrapper: bool,
}

impl Authoring {
    fn new(args: &crate::args::LiveArgs) -> Result<Self> {
        let runtime = args.runtime.canonicalize()?;
        let executable = match &args.worker {
            Some(path) => path.clone(),
            None => std::env::current_exe()?
                .with_file_name(format!("hew-live-host{}", crate::platform::exe_suffix())),
        };
        let directory = args
            .project
            .clone()
            .map_or_else(std::env::current_dir, Ok)?
            .canonicalize()?;
        let target = crate::target::TargetSpec::from_requested(None)?;
        let options = crate::compile::frontend_options(
            &target,
            &crate::compile::CompileOptions {
                project_dir: Some(directory.clone()),
                repl_fragment: true,
                ..Default::default()
            },
        );
        let compiler = hew_compile::Session::from_frontend_options(
            hew_compile::SessionTarget {
                hir_arch: crate::hir_target_arch(&target),
                ..hew_compile::SessionTarget::native()
            },
            &options,
        );
        let target = target.linker_triple();
        let timeout = crate::util::parse_timeout(&args.timeout)?;
        Ok(Self {
            frontend: LiveFrontend::new(directory.clone(), options),
            compiler,
            worker: Worker::start(&executable, &runtime, &target, timeout)?,
            artefacts: tempfile::Builder::new().prefix("hew-live-").tempdir()?,
            runtime,
            project: directory,
            target,
            opt_level: if args.release {
                hew_codegen_rs::OptLevel::O2
            } else {
                hew_codegen_rs::OptLevel::O0
            },
            next_generation: 0,
            modules: BTreeMap::new(),
            show_wrapper: args.show_wrapper,
        })
    }

    fn evaluate(&mut self, input: &str, kind: InputKind) -> Result<()> {
        let prepared = match self.frontend.prepare(&self.compiler, input, kind) {
            Ok(prepared) => prepared,
            Err(error) => {
                render_error(error);
                return Ok(());
            }
        };
        crate::compile::render_frontend_diagnostics(&prepared.diagnostics);
        if self.show_wrapper {
            eprintln!("// {}\n{}", prepared.path.display(), prepared.source);
        }
        let id = self.next_generation;
        self.next_generation = id.checked_add(1).ok_or("live generation IDs exhausted")?;
        let symbol = format!("submission_{id}");
        let module = self.emit(&prepared, &symbol)?;
        let admitted = self.worker.exchange(
            &json!({ "op": "prepare", "id": id, "module": module.0, "manifest": module.1 }),
        )?;
        if admitted["prepared"].as_u64() != Some(id) {
            return Err("live worker prepared an unexpected generation".into());
        }
        let generation = admitted["generation"]
            .as_u64()
            .ok_or("live worker omitted its generation identity")?;
        self.modules.insert(generation, module.0);
        let execution: Execution = self.worker.request(
            &json!({ "op": "execute", "id": id, "entry": symbol, "slot_count": prepared.slot_count() }),
            "execution",
        )?;
        let contents: Vec<_> = execution
            .slots
            .iter()
            .map(|slot| {
                if slot.initialized {
                    SessionContents::Full
                } else {
                    SessionContents::from_leaves(&slot.leaves)
                }
            })
            .collect();
        let retired = self.frontend.commit(prepared, &contents)?;
        if let Some(fault) = execution.fault {
            render_fault(&fault);
        }
        if !retired.is_empty() {
            let faults: Vec<Fault> = self.worker.request(
                &json!({ "op": "release", "slots": retired }),
                "cleanup_faults",
            )?;
            for fault in faults {
                render_fault(&fault);
            }
        }
        let collected = self
            .worker
            .request(&json!({ "op": "retire", "id": id }), "collected")?;
        self.remove_collected(collected)?;
        Ok(())
    }

    fn remove_collected(&mut self, collected: Vec<u64>) -> Result<()> {
        for generation in collected {
            if let Some(module) = self.modules.remove(&generation) {
                std::fs::remove_file(module)?;
            }
        }
        Ok(())
    }

    fn emit(
        &self,
        prepared: &PreparedInput,
        symbol: &str,
    ) -> Result<(PathBuf, hew_live::native::Manifest)> {
        let physical = crate::lower_session_to_physical(
            &prepared.live.output,
            &crate::target::TargetSpec::from_requested(None)?,
        )
        .map_err(|_| "live physical lowering failed")?;
        let exports = [(prepared.live.output.compiled_roots()[0], symbol)];
        let export = HostExport::new(&physical, &exports)?.with_live_code_ownership();
        let emitted = emit_host_object(
            &export,
            &PhysicalEmitOptions {
                module_name: symbol,
                out_dir: self.artefacts.path(),
                target_triple: Some(&self.target),
                opt_level: self.opt_level,
                emit_llvm: false,
                address_sanitizer: false,
                debug_source: None,
                link_freestanding_wasm: false,
                build_info: None,
            },
        )?;
        let object = emitted
            .native_obj_path
            .ok_or("live compiler produced no object")?;
        let module = self.artefacts.path().join(format!("{symbol}.module"));
        let manifest = hew_live::native::prepare(
            &object,
            &self.runtime,
            &module,
            hew_compile::VERSION,
            &self.target,
        )?;
        std::fs::remove_file(object)?;
        Ok((module, manifest))
    }

    fn finish(&mut self, operation: &str) -> Result<()> {
        let faults: Vec<Fault> = self
            .worker
            .request(&json!({ "op": operation }), "cleanup_faults")?;
        for fault in faults {
            render_fault(&fault);
        }
        Ok(())
    }
}

fn render_fault(fault: &Fault) {
    let message = fault.message.trim_end();
    if message.starts_with("hew: failure:") {
        eprintln!("{message}");
    } else {
        eprintln!("hew: failure: {message}");
    }
}

fn render_error(error: frontend::Error) {
    if let frontend::Error::Frontend(failure) = error {
        if crate::compile::render_frontend_diagnostics(&failure.diagnostics).is_none() {
            eprintln!("{failure}");
        }
    } else {
        eprintln!("{error}");
    }
}

fn handle_command(authoring: &mut Authoring, command: ReplCommand) -> Result<bool> {
    match command {
        ReplCommand::Quit => return Ok(true),
        ReplCommand::Help => {
            println!(":help, :quit, :session, :items, :clear, :type <expression>, :load <file>");
        }
        ReplCommand::Session => {
            for binding in authoring.frontend.bindings() {
                println!(
                    "{}: {} ({:?})",
                    binding.name,
                    binding.ty.user_facing(),
                    binding.contents
                );
            }
        }
        ReplCommand::Items => {
            for view in authoring
                .frontend
                .source_views()
                .values()
                .filter(|view| view.kind == InputKind::Declarations)
            {
                println!("{}", view.source);
            }
        }
        ReplCommand::Clear => {
            authoring.finish("clear")?;
            authoring.frontend.clear()?;
            let collected = authoring
                .worker
                .request(&json!({ "op": "collect" }), "collected")?;
            authoring.remove_collected(collected)?;
        }
        ReplCommand::Type(expression) => {
            let mut name = "__hew_live_type_query".to_string();
            while authoring
                .frontend
                .bindings()
                .iter()
                .any(|binding| binding.name == name)
            {
                name.push('_');
            }
            let source = format!("let {name} = {expression};");
            match authoring
                .frontend
                .prepare(&authoring.compiler, &source, InputKind::Statements)
            {
                Ok(prepared) => {
                    crate::compile::render_frontend_diagnostics(&prepared.diagnostics);
                    if let Some(binding) = prepared.live.bindings.last() {
                        println!("{}", binding.binding.ty.user_facing());
                    }
                }
                Err(error) => render_error(error),
            }
        }
        ReplCommand::Load(path) => {
            let source = std::fs::read_to_string(authoring.project.join(path))?;
            let kind = if classify::classify(&source) == classify::InputKind::Item {
                InputKind::Declarations
            } else {
                InputKind::Statements
            };
            authoring.evaluate(&source, kind)?;
        }
        ReplCommand::Unknown(command) => {
            eprintln!("unknown live command :{command}; use :help");
        }
    }
    Ok(false)
}

pub(crate) fn run(args: &crate::args::LiveArgs) -> Result<()> {
    let mut authoring = Authoring::new(args)?;
    let mut editor = rustyline::DefaultEditor::new()?;
    let prompt = if std::io::stdin().is_terminal() && std::io::stdout().is_terminal() {
        "hew live> "
    } else {
        ""
    };
    loop {
        let mut input = match editor.readline(prompt) {
            Ok(input) => input,
            Err(
                rustyline::error::ReadlineError::Eof | rustyline::error::ReadlineError::Interrupted,
            ) => break,
            Err(error) => return Err(error.into()),
        };
        while classify::input_completeness(&input) == InputCompleteness::Incomplete {
            let continuation = if prompt.is_empty() { "" } else { "... " };
            input.push('\n');
            input.push_str(&editor.readline(continuation)?);
        }
        let input = input.trim();
        if input.is_empty() {
            continue;
        }
        let _ = editor.add_history_entry(input);
        match classify::classify(input) {
            classify::InputKind::Command(command) => {
                if handle_command(&mut authoring, command)? {
                    break;
                }
            }
            classify::InputKind::Item => authoring.evaluate(input, InputKind::Declarations)?,
            classify::InputKind::Statement => authoring.evaluate(input, InputKind::Statements)?,
            classify::InputKind::Expression => authoring.evaluate(input, InputKind::Expression)?,
        }
    }
    authoring.finish("quit")
}
