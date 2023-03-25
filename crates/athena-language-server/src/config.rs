use serde::Deserialize;

#[derive(Deserialize, Debug, Clone, Default)]
pub struct Config {
    pub wip_goto_definition_enable: bool,

    pub wip_full_semantic_tokens_enable: bool,
}

impl From<Config> for ide::HighlightConfig {
    fn from(val: Config) -> ide::HighlightConfig {
        ide::HighlightConfig {
            with_name_res: val.wip_full_semantic_tokens_enable,
        }
    }
}
