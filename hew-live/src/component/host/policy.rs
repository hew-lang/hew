//! Finite host resource bounds shared by production and isolated previews.
//!
//! Admission counts follow actual retained work. Dropping a caller cannot
//! reclaim a running route's permit; a failed preparation releases its permits.
//! Fuel, issued service calls and output traffic are lifetime budgets of one
//! Store, including guest actors, tasks and other descendants. They do not reset
//! on each route. Exhaustion is an execution fault after admission; it cannot
//! undo effects already issued to a service.
//!
//! Linear memories, tables and WASI resources use Wasmtime's own limiters.
//! These are execution and admission bounds, not a process-wide resident-memory
//! guarantee. Engine compilation and trusted service implementation allocations
//! belong to the embedding process; component size and concurrent preparation
//! bound the work admitted to compilation.

use std::sync::{Arc, Mutex};
use std::time::Duration;

use tokio::sync::{OwnedSemaphorePermit, Semaphore};
use wasmtime::{StoreLimits, StoreLimitsBuilder};

use super::Error;

/// Bounds for one host and its descendant component instances. Memory and
/// table sizes apply to each guest memory/table; their counts bound the total.
/// Fuel and service traffic are cumulative per instance, including child work.
#[derive(Clone, Debug)]
pub struct HostPolicy {
    pub max_component_bytes: usize,
    pub max_prepared_components: usize,
    pub max_concurrent_preparations: usize,
    pub max_instances: usize,
    pub queue_capacity: usize,
    /// Queued and executing routes together, retained through caller loss.
    pub max_routes: usize,
    pub max_control_operations: usize,
    pub max_host_operations: usize,
    pub max_payload_bytes: usize,
    pub max_preview_fixtures: usize,
    pub max_service_calls: u64,
    /// Cumulative service traffic and exported guest error text.
    /// Issued effects are never credited back, including on provider failure.
    pub max_output_bytes: usize,
    pub host_operation_timeout: Duration,
    pub memory_bytes: usize,
    pub memories: usize,
    pub table_elements: usize,
    pub tables: usize,
    pub core_instances: usize,
    /// Per-instance WASI handles, including resources opened by descendants.
    pub max_host_resources: usize,
    pub fuel: u64,
}

impl Default for HostPolicy {
    fn default() -> Self {
        Self {
            max_component_bytes: 32 * 1024 * 1024,
            max_prepared_components: 16,
            max_concurrent_preparations: 2,
            max_instances: 8,
            queue_capacity: 64,
            max_routes: 64,
            max_control_operations: 64,
            max_host_operations: 32,
            max_payload_bytes: 1024 * 1024,
            max_preview_fixtures: 64,
            max_service_calls: 1_000_000,
            max_output_bytes: 8 * 1024 * 1024,
            host_operation_timeout: Duration::from_secs(30),
            memory_bytes: 128 * 1024 * 1024,
            memories: 4,
            table_elements: 100_000,
            tables: 16,
            core_instances: 64,
            max_host_resources: 1024,
            fuel: 10_000_000_000,
        }
    }
}

/// A resource refused before guest admission. Exhaustion after admission is a
/// guest execution fault, never a claim that previously issued effects rolled back.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum PolicyLimit {
    ComponentBytes,
    PreparedComponents,
    Preparations,
    Instances,
    ControlOperations,
    Routes,
    PayloadBytes,
    PreviewFixtures,
    HostOperations,
    ServiceCalls,
    OutputBytes,
}

impl HostPolicy {
    pub(super) fn validate(&self) -> Result<(), Error> {
        for (name, value) in [
            ("prepared components", self.max_prepared_components),
            ("concurrent preparations", self.max_concurrent_preparations),
            ("instances", self.max_instances),
            ("queue capacity", self.queue_capacity),
            ("routes", self.max_routes),
            ("control operations", self.max_control_operations),
            ("host operations", self.max_host_operations),
        ] {
            if value == 0 || value > Semaphore::MAX_PERMITS {
                return Err(Error::InvalidPolicy(name));
            }
        }
        if self.host_operation_timeout.is_zero()
            || tokio::time::Instant::now()
                .checked_add(self.host_operation_timeout)
                .is_none()
        {
            return Err(Error::InvalidPolicy("host operation timeout"));
        }
        Ok(())
    }

    pub(crate) fn payload(&self, lengths: &[usize]) -> Result<(), Error> {
        if lengths
            .iter()
            .try_fold(0_usize, |sum, length| sum.checked_add(*length))
            .is_none_or(|bytes| bytes > self.max_payload_bytes)
        {
            return Err(Error::Refused(PolicyLimit::PayloadBytes));
        }
        Ok(())
    }

    pub(crate) fn component(&self, bytes: usize) -> Result<(), Error> {
        if bytes > self.max_component_bytes {
            return Err(Error::Refused(PolicyLimit::ComponentBytes));
        }
        Ok(())
    }

    pub(super) fn store_limits(&self) -> StoreLimits {
        StoreLimitsBuilder::new()
            .memory_size(self.memory_bytes)
            .memories(self.memories)
            .table_elements(self.table_elements)
            .tables(self.tables)
            .instances(self.core_instances)
            .trap_on_grow_failure(true)
            .build()
    }
}

pub(super) struct Budgets {
    pub prepared: Arc<Semaphore>,
    pub preparing: Arc<Semaphore>,
    pub instances: Arc<Semaphore>,
    pub routes: Arc<Semaphore>,
    operations: Arc<Semaphore>,
}

impl Budgets {
    pub(super) fn new(policy: &HostPolicy) -> Self {
        Self {
            prepared: Arc::new(Semaphore::new(policy.max_prepared_components)),
            preparing: Arc::new(Semaphore::new(policy.max_concurrent_preparations)),
            instances: Arc::new(Semaphore::new(policy.max_instances)),
            routes: Arc::new(Semaphore::new(policy.max_routes)),
            operations: Arc::new(Semaphore::new(policy.max_host_operations)),
        }
    }
}

pub(super) fn reserve(
    semaphore: &Arc<Semaphore>,
    limit: PolicyLimit,
) -> Result<OwnedSemaphorePermit, Error> {
    Arc::clone(semaphore)
        .try_acquire_owned()
        .map_err(|_| Error::Refused(limit))
}

pub(super) fn fault(limit: PolicyLimit) -> wasmtime::Error {
    wasmtime::Error::msg(format!("component host policy exhausted: {limit:?}"))
}

/// One instance's service traffic; preparation and preview use the same host
/// concurrency pool. Providers are trusted host code: their own implementation
/// allocations are outside guest memory, and a timeout cannot undo their effects.
pub(super) struct ServicesBudget {
    policy: Arc<HostPolicy>,
    operations: Arc<Semaphore>,
    remaining: Mutex<(u64, usize)>,
}

impl ServicesBudget {
    pub(super) fn new(policy: Arc<HostPolicy>, budgets: &Budgets) -> Self {
        Self {
            remaining: Mutex::new((policy.max_service_calls, policy.max_output_bytes)),
            policy,
            operations: Arc::clone(&budgets.operations),
        }
    }

    pub(super) fn begin(&self, lengths: &[usize]) -> wasmtime::Result<OwnedSemaphorePermit> {
        self.policy
            .payload(lengths)
            .map_err(|_| fault(PolicyLimit::PayloadBytes))?;
        let permit = reserve(&self.operations, PolicyLimit::HostOperations)
            .map_err(|_| fault(PolicyLimit::HostOperations))?;
        let bytes: usize = lengths.iter().sum(); // payload proved a bounded sum
        let mut remaining = self
            .remaining
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if remaining.0 == 0 {
            return Err(fault(PolicyLimit::ServiceCalls));
        }
        if bytes > remaining.1 {
            return Err(fault(PolicyLimit::OutputBytes));
        }
        remaining.0 -= 1;
        remaining.1 -= bytes;
        Ok(permit)
    }

    pub(super) fn response(&self, bytes: usize) -> wasmtime::Result<()> {
        self.policy
            .payload(&[bytes])
            .map_err(|_| fault(PolicyLimit::PayloadBytes))?;
        let mut remaining = self
            .remaining
            .lock()
            .unwrap_or_else(std::sync::PoisonError::into_inner);
        if bytes > remaining.1 {
            return Err(fault(PolicyLimit::OutputBytes));
        }
        remaining.1 -= bytes;
        Ok(())
    }

    pub(super) async fn complete<T>(
        &self,
        operation: super::ServiceCall<T>,
    ) -> wasmtime::Result<Result<T, String>> {
        tokio::time::timeout(self.policy.host_operation_timeout, operation)
            .await
            .map_err(|_| {
                wasmtime::Error::msg(
                    "component host operation expired; prior effects are not rolled back",
                )
            })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn shared_operation_permits_refund_without_resetting_issued_traffic() {
        let policy = Arc::new(HostPolicy {
            max_host_operations: 1,
            max_service_calls: 2,
            max_output_bytes: 8,
            ..HostPolicy::default()
        });
        let budgets = Budgets::new(&policy);
        let first = ServicesBudget::new(Arc::clone(&policy), &budgets);
        let other_instance = ServicesBudget::new(policy, &budgets);
        let permit = first.begin(&[2]).unwrap();
        assert!(other_instance.begin(&[1]).is_err());
        drop(permit);
        drop(other_instance.begin(&[1]).unwrap());
        drop(first.begin(&[3]).unwrap());
        assert!(first.response(4).is_err());
        first.response(3).unwrap();
        assert!(first.begin(&[0]).is_err());
    }
}
