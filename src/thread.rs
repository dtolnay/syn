use std::fmt::{self, Debug};
use std::panic::{self, PanicInfo};
use std::thread::{self, ThreadId};

/// ThreadBound is a Sync-maker and Send-maker that allows accessing a value
/// of type T only from the original thread on which the ThreadBound was
/// constructed.
pub struct ThreadBound<T> {
    value: T,
    // None if constructed in atexit context after the standard library's thread
    // local data has already been torn down.
    thread_id: Option<ThreadId>,
}

unsafe impl<T> Sync for ThreadBound<T> {}

// Send bound requires Copy, as otherwise Drop could run in the wrong place.
unsafe impl<T: Copy> Send for ThreadBound<T> {}

impl<T> ThreadBound<T> {
    pub fn new(value: T) -> Self {
        ThreadBound {
            value,
            thread_id: thread_id_if_possible(),
        }
    }

    pub fn get(&self) -> Option<&T> {
        if thread_id_if_possible() == self.thread_id {
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

impl<T: Clone> Clone for ThreadBound<T> {
    fn clone(&self) -> Self {
        ThreadBound {
            value: self.value.clone(),
            thread_id: self.thread_id,
        }
    }
}

fn thread_id_if_possible() -> Option<ThreadId> {
    type PanicHook = dyn Fn(&PanicInfo) + Sync + Send + 'static;

    let null_hook: Box<PanicHook> = Box::new(|_panic_info| { /* ignore */ });
    let sanity_check = &*null_hook as *const PanicHook;
    let original_hook = panic::take_hook();
    panic::set_hook(null_hook);

    let thread_id = match panic::catch_unwind(thread::current) {
        Ok(thread) => Some(thread.id()),
        Err(_panic) => None,
    };

    let hopefully_null_hook = panic::take_hook();
    panic::set_hook(original_hook);
    if sanity_check != &*hopefully_null_hook {
        panic!("observed race condition in syn::thread::ThreadBound");
    }

    thread_id
}
