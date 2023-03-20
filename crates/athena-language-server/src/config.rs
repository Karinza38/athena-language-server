use serde::Deserialize;

#[derive(Deserialize, Debug, Clone, Default)]
pub struct Config {
    pub wip_goto_definition_enable: bool,
}
