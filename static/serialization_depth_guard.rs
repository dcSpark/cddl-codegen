// Opt-in recursion depth guard (generated only under `--deserialize-depth-limit`). Composite
// deserializers are recursive-descent, so a recursive type (e.g. `tree = [value: uint,
// children: [* tree]]`) has no intrinsic bound: hostile deeply-nested CBOR would recurse until the
// stack overflows and the process aborts (SIGABRT, uncatchable). Each generated composite
// `deserialize` acquires this guard at entry; exceeding the baked-in limit returns a graceful
// `DeserializeError` instead.
thread_local! {
    static DESERIALIZE_DEPTH: std::cell::Cell<usize> = std::cell::Cell::new(0);
}

/// RAII guard bounding the nesting depth of composite deserializers. `acquire` increments a
/// thread-local counter and `Drop` decrements it, so early returns / `?`-propagation unwind the
/// count correctly. Acquiring past `limit` returns a `DepthLimitExceeded` error rather than
/// recursing further.
pub struct DepthGuard;

impl DepthGuard {
    pub fn acquire(limit: usize) -> Result<Self, DeserializeError> {
        DESERIALIZE_DEPTH.with(|depth| {
            let next = depth.get() + 1;
            if next > limit {
                Err(DeserializeError::from(
                    DeserializeFailure::DepthLimitExceeded { limit },
                ))
            } else {
                depth.set(next);
                Ok(DepthGuard)
            }
        })
    }
}

impl Drop for DepthGuard {
    fn drop(&mut self) {
        DESERIALIZE_DEPTH.with(|depth| depth.set(depth.get() - 1));
    }
}
