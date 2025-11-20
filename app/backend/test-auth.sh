#!/bin/bash

# Login
echo "=== Login ==="
LOGIN_RESPONSE=$(curl -s -X POST http://localhost:3000/api/auth/login \
  -H "Content-Type: application/json" \
  -d '{"userId":"admin-001","password":"admin123"}')

echo "$LOGIN_RESPONSE" | jq .

# Extract token
TOKEN=$(echo "$LOGIN_RESPONSE" | jq -r .token)
echo ""
echo "Token: $TOKEN"
echo ""

# Test /me endpoint
echo "=== Test /api/auth/me ==="
curl -s -X GET http://localhost:3000/api/auth/me \
  -H "Authorization: Bearer $TOKEN" | jq .
