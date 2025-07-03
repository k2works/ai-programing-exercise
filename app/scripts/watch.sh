#!/usr/bin/env bash
# cargo-watch用のスクリプト

# cargo watchをインストールする
if ! command -v cargo-watch &> /dev/null; then
    echo "cargo-watchをインストールしています..."
    cargo install cargo-watch
fi

# rust-clippy（静的解析）をインストールする
if ! command -v cargo-clippy &> /dev/null; then
    echo "rust-clippyをインストールしています..."
    rustup component add clippy
fi

# tarpaulin（コードカバレッジ）をインストールする
if ! command -v cargo-tarpaulin &> /dev/null; then
    echo "cargo-tarpaulinをインストールしています..."
    cargo install cargo-tarpaulin
fi

# watchコマンドを実行
cargo watch \
    -x "fmt --all -- --check" \
    -x "clippy -- -D warnings" \
    -x test \
    -x "tarpaulin --config tarpaulin.toml"
