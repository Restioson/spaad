extern crate spaad_internal;

#[doc(inline)]
pub use spaad_internal::entangled;

#[doc(hidden)]
pub mod export {
    pub use xtra;
    #[cfg(feature = "stable")]
    pub use async_trait;
}
