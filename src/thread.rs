use std::fmt::{self, Debug};
use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicUsize, Ordering};

/// ThreadBound is a Sync-maker and Send-maker that allows accessing a value
/// of type T only from the original thread on which the ThreadBound was
/// constructed.
pub struct ThreadBound<T> {
    value: T,
    thread_id: ThreadId,
}

unsafe impl<T> Sync for ThreadBound<T> {}

// Send bound requires Copy, as otherwise Drop could run in the wrong place.
unsafe impl<T: Copy> Send for ThreadBound<T> {}

impl<T> ThreadBound<T> {
    pub fn new(value: T) -> Self {
        ThreadBound {
            value,
            thread_id: ThreadId::current(),
        }
    }

    pub fn get(&self) -> Option<&T> {
        if ThreadId::current() == self.thread_id {
            Some(&self.value)
        } else {
            None
        }
    }
}

impl<T: Debug> Debug for ThreadBound<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self.get() {
            Some(value) => Debug::fmt(value, formatter),
            None => formatter.write_str("unknown"),
        }
    }
}

#[derive(Copy, Clone, PartialEq)]
struct ThreadId(NonZeroUsize);

impl ThreadId {
    fn current() -> Self {
        static NEXT_THREAD_ID: AtomicUsize = AtomicUsize::new(1);

        thread_local! {
            static THIS_THREAD_ID: ThreadId = {
                let mut current = NEXT_THREAD_ID.load(Ordering::Relaxed);
                loop {
                    let next = current.checked_add(1).expect("ThreadId overflow");
                    match NEXT_THREAD_ID.compare_exchange_weak(current, next, Ordering::Relaxed, Ordering::Relaxed) {
                        Ok(current) => break ThreadId(unsafe { NonZeroUsize::new_unchecked(current) }),
                        Err(update) => current = update,
                    }
                }
            };
        }

        THIS_THREAD_ID.with(ThreadId::clone)
    }
}
