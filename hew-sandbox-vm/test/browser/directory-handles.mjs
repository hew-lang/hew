// Run in a secure browser origin with the built compiler and VM modules.
// These are real OPFS entries; no virtual filesystem or picker is installed.
export async function runDirectoryHandleCases(compiler, vm, source) {
  const assert = (condition, message) => {
    if (!condition) throw new Error(message);
  };
  const compile = (source) => {
    const output = JSON.parse(
      compiler.compileToSandboxBytecode(source, "sandbox-vm-export"),
    );
    assert(output.bytecode, JSON.stringify(output.diagnostics));
    return output.bytecode;
  };
  const package_ = compile(source);
  const storage = await navigator.storage.getDirectory();
  const name = `hew-directory-${crypto.randomUUID()}`;
  const base = await storage.getDirectoryHandle(name, { create: true });
  const root = await base.getDirectoryHandle("root", { create: true });
  const outside = await base.getDirectoryHandle("outside", { create: true });
  const put = async (directory, path, data) => {
    const file = await directory.getFileHandle(path, { create: true });
    const writer = await file.createWritable();
    await writer.write(data);
    await writer.close();
  };
  try {
    await put(outside, "value", "outside");
    const child = await root.getDirectoryHandle("child", { create: true });
    await child.getDirectoryHandle("nest", { create: true });
    await put(child, "value", "inside");
    const payload = Uint8Array.from(
      { length: 65536 },
      (_, index) => index % 251,
    );
    await put(root, "payload", payload);
    await put(root, "empty", new Uint8Array());
    const options = {
      directoryRoots: { read: root, write: root },
      stepBudget: 1000000,
    };
    const refused = await vm.runBytecode(package_);
    assert(
      refused.result === "sandbox_rejected",
      "absent host authority must reject before execution",
    );
    assert(
      refused.final_state.sandbox_rejections[0].message.includes(
        "host-supplied OPFS",
      ),
      "missing host authority diagnostic is inaccurate",
    );
    assert(
      refused.final_state.stdout.length === 0,
      "default-deny execution produced output",
    );
    const trace = await vm.runBytecode(package_, options);
    assert(trace.result === "ok", JSON.stringify(trace.final_state));
    assert(
      trace.final_state.stdout.join("") ===
        "closed cancelled root\nbrowser directory remains rooted\n",
      "unexpected source cleanup or result",
    );
    const copied = new Uint8Array(
      await (
        await (await root.getFileHandle("copied")).getFile()
      ).arrayBuffer(),
    );
    assert(
      copied.length === payload.length &&
        copied.every((value, i) => value === payload[i]),
      "source write did not reach real browser storage",
    );
    assert(
      (await (
        await (await outside.getFileHandle("value")).getFile()
      ).text()) === "outside",
      "outside-root entry changed",
    );
    const malformed = structuredClone(package_);
    const acquisition = malformed.functions
      .flatMap((fn) => fn.blocks)
      .find((block) => block.term.result_resource != null);
    delete acquisition.term.result_resource;
    const rejected = await vm.runBytecode(malformed, options);
    assert(
      rejected.result === "sandbox_rejected",
      "missing compiler owner slot must reject",
    );
    acquisition.term.result_resource = malformed.resources.findIndex(
      (resource) =>
        resource.kind === "nominal" &&
        resource.release === "hew_file_read_close",
    );
    const wrongOwner = await vm.runBytecode(malformed, options);
    assert(
      wrongOwner.result === "sandbox_rejected" &&
        wrongOwner.final_state.step_count === 0,
      "incompatible compiler owner release must reject before effects",
    );
    const cancellations = [];
    for (const access of ["read", "write"]) {
      const type = access === "read" ? "ReadDir" : "WriteDir";
      const operation =
        access === "read"
          ? 'let _ = root.directory.read("payload").expect("read");'
          : 'root.directory.write("cancelled", "uncommitted".to_bytes()).expect("write");';
      const bytecode = compile(`import std.fs;
        #[resource] type Root { directory: fs.${type}; }
        impl Root { fn close(consume self) {
          let Root { directory } = self; directory.close(); println("cancelled ${access} root closed");
        } }
        fn main() {
          let root = Root { directory: fs.open_${access}_dir(".").expect("root") };
          ${operation}
          println("unexpected host continuation");
        }`);
      let entered;
      let release;
      const started = new Promise((resolve) => {
        entered = resolve;
      });
      const gate = new Promise((resolve) => {
        release = resolve;
      });
      const controller = new AbortController();
      const prototype =
        access === "read"
          ? FileSystemFileHandle.prototype
          : FileSystemWritableFileStream.prototype;
      const method = access === "read" ? "getFile" : "write";
      const original = prototype[method];
      // The real host operation runs first; only delivery of its completion is
      // held. This exercises borrowed input retention, not a fake filesystem.
      prototype[method] = async function (...args) {
        const result = await original.apply(this, args);
        entered();
        await gate;
        return result;
      };
      let execution;
      try {
        execution = vm.runBytecode(bytecode, {
          ...options,
          signal: controller.signal,
        });
        let finished = false;
        execution.then(() => {
          finished = true;
        });
        await started;
        controller.abort();
        await Promise.resolve();
        assert(
          !finished,
          "cancellation released a still-borrowed host operation",
        );
        release();
        const cancelled = await execution;
        assert(
          cancelled.final_state.stdout.join("") ===
            `cancelled ${access} root closed\n`,
          JSON.stringify(cancelled.final_state),
        );
        assert(
          cancelled.result !== "ok",
          "cancelled operation unexpectedly succeeded",
        );
        cancellations.push(access);
      } finally {
        release();
        if (execution) await execution;
        prototype[method] = original;
      }
    }
    assert(
      (await (
        await (await root.getFileHandle("cancelled")).getFile()
      ).text()) === "",
      "cancelled writer published uncommitted data",
    );
    const budget = await checkBudgetRetirement(
      compile,
      vm,
      options,
      root,
      assert,
    );
    return {
      budget,
      result: "passed",
      stdout: trace.final_state.stdout,
      bytes: copied.length,
      noAuthority: refused.result,
      missingOwner: rejected.result,
      wrongOwner: wrongOwner.result,
      cancellations,
    };
  } finally {
    await storage.removeEntry(name, { recursive: true });
  }
}

async function checkBudgetRetirement(compile, vm, options, root, assert) {
  const { BrowserDirectories } =
    await import("../../dist/interpreter/v1/browser-directory.js");
  const { FrameScheduler } =
    await import("../../dist/interpreter/v1/scheduler.js");
  const bytecode = compile(`import std.fs;
    fn write_pending() -> i64 {
      let directory = fs.open_write_dir(".").expect("write root");
      directory.write("budget", "uncommitted".to_bytes()).expect("write");
      println("unexpected write continuation"); 1
    }
    fn acquire_pending() -> i64 {
      let directory = fs.open_read_dir(".").expect("read root");
      let child = directory.subdir("child").expect("child root");
      println("unexpected acquisition continuation"); child.read("value").expect("child").len()
    }
    fn exhaust_after_read() -> i64 {
      let directory = fs.open_read_dir(".").expect("read root");
      let _ = directory.read("empty").expect("read");
      while true {}
      0
    }
    fn main() { let _ = await race [write_pending(), acquire_pending(), exhaust_after_read()]; }
  `);
  let release;
  let enteredWrite;
  let enteredAcquisition;
  let retiring;
  const gate = new Promise((resolve) => {
    release = resolve;
  });
  const writing = new Promise((resolve) => {
    enteredWrite = resolve;
  });
  const acquiring = new Promise((resolve) => {
    enteredAcquisition = resolve;
  });
  const terminal = new Promise((resolve) => {
    retiring = resolve;
  });
  const originals = {
    write: FileSystemWritableFileStream.prototype.write,
    getFile: FileSystemFileHandle.prototype.getFile,
    getDirectory: FileSystemDirectoryHandle.prototype.getDirectoryHandle,
    retire: FrameScheduler.prototype.retireHost,
    call: BrowserDirectories.prototype.call,
  };
  const owners = new Set();
  const closes = new Map();
  FileSystemWritableFileStream.prototype.write = async function (...args) {
    const result = await originals.write.apply(this, args);
    enteredWrite();
    await gate;
    return result;
  };
  FileSystemDirectoryHandle.prototype.getDirectoryHandle = async function (
    name,
    ...args
  ) {
    const handle = await originals.getDirectory.call(this, name, ...args);
    if (name === "child") {
      enteredAcquisition();
      await gate;
    }
    return handle;
  };
  FileSystemFileHandle.prototype.getFile = async function (...args) {
    const file = await originals.getFile.apply(this, args);
    if (this.name === "empty") await Promise.all([writing, acquiring]);
    return file;
  };
  FrameScheduler.prototype.retireHost = function (...args) {
    retiring();
    return originals.retire.apply(this, args);
  };
  BrowserDirectories.prototype.call = function (operation, args, ...rest) {
    const result = originals.call.call(this, operation, args, ...rest);
    if (operation.action === "close")
      closes.set(args[0], (closes.get(args[0]) ?? 0) + 1);
    if (result instanceof Promise)
      void result.then((outcome) => {
        if (outcome.value.kind === "host_resource") owners.add(outcome.value);
      });
    return result;
  };
  let execution;
  try {
    execution = vm.runBytecode(bytecode, { ...options, stepBudget: 20000 });
    let finished = false;
    execution.then(() => {
      finished = true;
    });
    await terminal;
    assert(!finished, "budget exhaustion returned before host quiescence");
    release();
    const trace = await execution;
    assert(
      trace.result === "budget_exhausted",
      JSON.stringify(trace.final_state),
    );
    assert(
      trace.final_state.stdout.length === 0,
      "a retired host continuation executed source instructions",
    );
    assert(
      trace.final_state.budget_remaining === 0,
      "budget failure changed during host retirement",
    );
    assert(
      owners.size >= 6,
      "the budget oracle did not acquire its real directory/file owners",
    );
    assert(
      [...owners].every(
        (value) => value.owner.closed && closes.get(value) === 1,
      ),
      "terminal retirement did not close each selected owner exactly once",
    );
    assert(
      (await (await (await root.getFileHandle("budget")).getFile()).text()) ===
        "",
      "budget cancellation published uncommitted bytes",
    );
    return trace.result;
  } finally {
    release();
    if (execution) await execution;
    FileSystemWritableFileStream.prototype.write = originals.write;
    FileSystemFileHandle.prototype.getFile = originals.getFile;
    FileSystemDirectoryHandle.prototype.getDirectoryHandle =
      originals.getDirectory;
    FrameScheduler.prototype.retireHost = originals.retire;
    BrowserDirectories.prototype.call = originals.call;
  }
}
