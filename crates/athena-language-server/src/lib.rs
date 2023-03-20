pub mod config;
pub mod from_proto;
pub mod semantic_tokens;
pub mod to_proto;

mod line_index;

mod global_state;
mod handlers;
mod main_loop;

pub use main_loop::run_server;

pub use anyhow::Result;
