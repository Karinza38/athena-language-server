use base_db::salsa::{Database, Durability};

use crate::RootDatabase;

impl RootDatabase {
    pub fn request_cancellation(&mut self) {
        self.salsa_runtime_mut().synthetic_write(Durability::LOW);
    }
}
