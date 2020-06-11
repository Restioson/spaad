extern crate spaad_internal;

#[doc(inline)]
pub use spaad_internal::*;

#[doc(hidden)]
pub mod export {
    #[cfg(not(feature = "nightly"))]
    pub use async_trait;
    pub use xtra;
}
