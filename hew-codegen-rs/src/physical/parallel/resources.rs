//! Resource admission for parallel Linux code generation.

use std::fs;
use std::path::{Component, Path};

const MIB: u64 = 1024 * 1024;
const SPLIT_RESERVE: u64 = 2_048 * MIB;
const WORKER_RESERVE: u64 = 512 * MIB;

pub(super) struct Budget {
    cpus: usize,
    memory: u64,
}

impl Budget {
    // JUSTIFIED: Unknown host capacity opts out of parallel emission.
    pub(super) fn read() -> Option<Self> {
        let cpus = std::thread::available_parallelism().ok()?.get();
        if cpus < 2 {
            return None;
        }
        // A process limit may make the host and cgroup readings misleading.
        let limits = fs::read_to_string("/proc/self/limits").ok()?;
        if ["Max address space", "Max data size"].iter().any(|name| {
            limits.lines().find_map(|line| {
                line.strip_prefix(name)
                    .and_then(|value| value.split_whitespace().next())
            }) != Some("unlimited")
        }) {
            return None;
        }
        let meminfo = fs::read_to_string("/proc/meminfo").ok()?;
        let host_kib: u64 = meminfo
            .lines()
            .find_map(|line| line.strip_prefix("MemAvailable:"))?
            .split_whitespace()
            .next()?
            .parse()
            .ok()?;
        let cgroups = fs::read_to_string("/proc/self/cgroup").ok()?;
        // A v1 memory controller needs separate limit discovery.
        if cgroups.lines().any(|line| {
            line.split(':')
                .nth(1)
                .is_some_and(|controllers| controllers.split(',').any(|c| c == "memory"))
        }) {
            return None;
        }
        let membership = cgroups.lines().find_map(|line| line.strip_prefix("0::/"))?;
        let mountinfo = fs::read_to_string("/proc/self/mountinfo").ok()?;
        if !mountinfo.lines().any(|line| {
            let Some((details, filesystem)) = line.split_once(" - ") else {
                return false;
            };
            let mut fields = details.split_whitespace().skip(3);
            filesystem.starts_with("cgroup2 ")
                && fields.next() == Some("/")
                && fields.next() == Some("/sys/fs/cgroup")
        }) {
            return None;
        }
        let memory = host_kib.checked_mul(1024)?.min(cgroup_headroom(
            Path::new("/sys/fs/cgroup"),
            Path::new(membership),
        )?);
        let budget = Self { cpus, memory };
        budget.workers(0).map(|_| budget)
    }

    // JUSTIFIED: A snapshot too large to budget requires monolithic emission.
    pub(super) fn workers(&self, snapshot_bytes: usize) -> Option<usize> {
        // Serialized bitcode underestimates live LLVM IR. Reserve room for
        // parsing/cloning it and for the independent target machines. These
        // margins intentionally favour the ordinary path under pressure.
        let split =
            SPLIT_RESERVE.checked_add(u64::try_from(snapshot_bytes).ok()?.checked_mul(24)?)?;
        [4, 2].into_iter().find(|&workers| {
            workers <= self.cpus
                && self.memory.saturating_sub(split) >= workers as u64 * WORKER_RESERVE
        })
    }
}

// JUSTIFIED: Incomplete cgroup accounting opts out of parallel emission.
fn cgroup_headroom(root: &Path, membership: &Path) -> Option<u64> {
    if membership
        .components()
        .any(|component| !matches!(component, Component::Normal(_)))
    {
        return None;
    }
    let directory = root.join(membership);
    let mut headroom = u64::MAX;
    for group in directory
        .ancestors()
        .take_while(|group| group.starts_with(root))
    {
        let mut bound = u64::MAX;
        for name in ["memory.max", "memory.high"] {
            match fs::read_to_string(group.join(name)) {
                Ok(value) if value.trim() == "max" => {}
                Ok(value) => bound = bound.min(value.trim().parse().ok()?),
                // The top-level system cgroup omits these files.
                Err(error) if group == root && error.kind() == std::io::ErrorKind::NotFound => {}
                Err(_) => return None,
            }
        }
        if bound != u64::MAX {
            let used = fs::read_to_string(group.join("memory.current"))
                .ok()?
                .trim()
                .parse()
                .ok()?;
            headroom = headroom.min(bound.saturating_sub(used));
        }
    }
    Some(headroom)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parallelism_requires_cpu_and_headroom_for_the_module() {
        let mut budget = Budget {
            cpus: 1,
            memory: 16_384 * MIB,
        };
        assert_eq!(budget.workers(64 * 1024 * 1024), None);
        budget.cpus = 16;
        assert_eq!(budget.workers(64 * 1024 * 1024), Some(4));
        budget.memory = 4_700 * MIB;
        assert_eq!(budget.workers(64 * 1024 * 1024), Some(2));
        assert_eq!(budget.workers(128 * 1024 * 1024), None);
    }

    #[test]
    fn headroom_respects_parent_pressure_and_unknown_accounting() {
        let root = tempfile::tempdir().unwrap();
        let child = root.path().join("parent/child");
        fs::create_dir_all(&child).unwrap();
        for (dir, high, used) in [
            (child.parent().unwrap(), "800", "720"),
            (&child, "max", "31"),
        ] {
            fs::write(dir.join("memory.max"), "900").unwrap();
            fs::write(dir.join("memory.high"), high).unwrap();
            fs::write(dir.join("memory.current"), used).unwrap();
        }
        let membership = Path::new("parent/child");
        assert_eq!(cgroup_headroom(root.path(), membership), Some(80));
        fs::remove_file(child.join("memory.current")).unwrap();
        assert_eq!(cgroup_headroom(root.path(), membership), None);
    }
}
