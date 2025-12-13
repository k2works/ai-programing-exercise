# ビルドステージ
FROM rust:1.85-alpine AS builder

# 必要なビルドツールをインストール
RUN apk add --no-cache musl-dev pkgconfig openssl-dev perl make

WORKDIR /app

# ワークスペース全体をコピー
COPY Cargo.toml ./
COPY Cargo.lock ./
COPY shared ./shared
COPY financial_accounting ./financial_accounting
COPY management_accounting ./management_accounting
COPY api_gateway ./api_gateway

# 財務会計サービスをビルド
WORKDIR /app/financial_accounting
ENV SQLX_OFFLINE=true
RUN cargo update -p base64ct --precise 1.6.0 && \
    cargo update -p home --precise 0.5.9 && \
    cargo build --release

# 実行ステージ
FROM alpine:latest

# 実行時に必要なライブラリをインストール
RUN apk add --no-cache ca-certificates libgcc

WORKDIR /app

# ビルドステージから実行ファイルをコピー
COPY --from=builder /app/target/release/financial_accounting /app/financial_accounting

# マイグレーションファイルをコピー（必要な場合）
COPY financial_accounting/migrations ./migrations

EXPOSE 8080

CMD ["/app/financial_accounting"]
