pub mod from_proto;
pub mod semantic_tokens;
pub mod to_proto;

mod line_index;

mod handlers;
mod main_loop;
mod global_state;

pub use main_loop::run_server;

pub use anyhow::Result;
