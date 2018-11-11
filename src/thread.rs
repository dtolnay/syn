use std::fmt::{self, Debug};

#[cfg(syn_can_use_thread_id)]
mod imp {
    use std::thread::{self, ThreadId};

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
                thread_id: thread::current().id(),
            }
        }

        pub fn get(&self) -> Option<&T> {
            if thread::current().id() == self.thread_id {
                Some(&self.value)
            } else {
                None
            }
        }
    }
}

#[cfg(not(syn_can_use_thread_id))]
mod imp {
    pub struct ThreadBound<T> {
        value: T,
    }

    impl<T> ThreadBound<T> {
        pub fn new(value: T) -> Self {
            ThreadBound {
                value: value,
            }
        }

        pub fn get(&self) -> Option<&T> {
            Some(&self.value)
        }
    }
}

pub use self::imp::ThreadBound;

impl<T: Debug> Debug for ThreadBound<T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self.get() {
            Some(value) => Debug::fmt(value, formatter),
            None => formatter.write_str("unknown"),
        }
    }
}
