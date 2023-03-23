use serde::Deserialize;

#[derive(Deserialize, Debug, Clone, Default)]
pub struct Config {
    pub wip_goto_definition_enable: bool,

    pub wip_full_semantic_tokens_enable: bool,
}

impl Into<ide::HighlightConfig> for Config {
    fn into(self) -> ide::HighlightConfig {
        ide::HighlightConfig {
            with_name_res: self.wip_full_semantic_tokens_enable,
        }
    }
}
