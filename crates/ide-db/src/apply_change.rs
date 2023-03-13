use base_db::{
    salsa::{Database, Durability},
    Change,
};

use crate::RootDatabase;

impl RootDatabase {
    pub fn request_cancellation(&mut self) {
        self.salsa_runtime_mut().synthetic_write(Durability::LOW);
    }

    pub fn apply_change(&mut self, change: Change) {
        // cancel executing queries, as they're about to become invalid
        self.request_cancellation();

        // TODO: in the future potentially update the source roots

        // apply it
        change.apply(self);
    }
}
