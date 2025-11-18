FROM nginx:alpine

# カスタム nginx.conf をコピー
COPY nginx.conf /etc/nginx/nginx.conf

# ヘルスチェック
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
  CMD wget --quiet --tries=1 --spider http://localhost/health || exit 1

EXPOSE 80

CMD ["nginx", "-g", "daemon off;"]
