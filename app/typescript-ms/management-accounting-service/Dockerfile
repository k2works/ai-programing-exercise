FROM node:20-alpine AS builder

WORKDIR /app

# 依存関係のインストール
COPY package*.json ./
COPY prisma ./prisma/
RUN npm ci

# ソースコードのコピーとビルド
COPY . .
RUN npx prisma generate
RUN npm run build

# 本番用イメージ
FROM node:20-alpine

WORKDIR /app

# 本番依存関係のみインストール
COPY package*.json ./
RUN npm ci --only=production

# ビルド成果物とPrismaスキーマをコピー
COPY --from=builder /app/dist ./dist
COPY --from=builder /app/prisma ./prisma
COPY --from=builder /app/node_modules/.prisma ./node_modules/.prisma

# 環境変数
ENV NODE_ENV=production
ENV PORT=3002

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD node -e "require('http').get('http://localhost:3002/health', (r) => {process.exit(r.statusCode === 200 ? 0 : 1)})"

EXPOSE 3002

CMD ["node", "dist/server.js"]
