use std::fmt::{self, Debug};

use self::thread_id::ThreadId;

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
            value: value,
            thread_id: thread_id::current(),
        }
    }

    pub fn get(&self) -> Option<&T> {
        if thread_id::current() == self.thread_id {
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

mod thread_id {
    use std::thread;

    pub use std::thread::ThreadId;

    pub fn current() -> ThreadId {
        thread::current().id()
    }
}
