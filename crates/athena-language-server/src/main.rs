#[tokio::main]
async fn main() {
    athena_language_server::run_server().await;
}
