use crate::{from_proto, to_proto, Result};
use crate::{global_state::GlobalStateSnapshot, semantic_tokens};
use anyhow::Context;
use ide::Cancellable;
use ide_db::base_db::FileRange;
use tower_lsp::lsp_types::{
    GotoDefinitionParams, GotoDefinitionResponse, SemanticTokensParams, SemanticTokensResult,
};

pub(crate) fn semantic_tokens_full(
    snapshot: GlobalStateSnapshot,
    params: SemanticTokensParams,
) -> Result<Option<SemanticTokensResult>> {
    let analysis = &snapshot.analysis;

    let file_id = snapshot
        .file_id(&params.text_document.uri)
        .with_context(|| format!("failed to get file id for uri {}", params.text_document.uri))?;
    let uri = params.text_document.uri.to_string();

    let semantic_tokens = snapshot.semantic_token_map.get(&uri).map(|v| v.clone());
    if let Some(semantic_tokens) = semantic_tokens {
        return Ok(Some(SemanticTokensResult::Tokens(semantic_tokens)));
    } else {
        let index = analysis.file_line_index(file_id)?;
        let ast = analysis.parse(file_id)?;

        let tokens = semantic_tokens::semantic_tokens_for_file(ast.tree(), &index);
        snapshot.semantic_token_map.insert(uri, tokens.clone());
        return Ok(Some(SemanticTokensResult::Tokens(tokens)));
    }
}

pub(crate) fn go_to_definition(
    snapshot: GlobalStateSnapshot,
    params: GotoDefinitionParams,
) -> Result<Option<GotoDefinitionResponse>> {
    let file_position = from_proto::file_position(&snapshot, params.text_document_position_params)?;

    let analysis = &snapshot.analysis;
    let Some(navs) = analysis.go_to_definition(file_position)? else {
        return Ok(None);
    };

    let locations = navs
        .info
        .into_iter()
        .map(|nav| {
            to_proto::location(
                &snapshot,
                FileRange {
                    file_id: nav.file_id,
                    range: nav.focus_range.unwrap_or_else(|| nav.full_range),
                },
            )
        })
        .collect::<Cancellable<Vec<_>>>()?;

    Ok(Some(locations.into()))
}
