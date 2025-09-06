#!/bin/bash
set -e

BASE_URL="${BASE_URL:-http://host.docker.internal:5080}"
API_BASE="$BASE_URL/api"

echo "Seeding test data for load testing..."
echo "Target API: $API_BASE"

# Test users
declare -A users=(
    ["admin@example.com"]="Admin123!"
    ["user1@example.com"]="User123!"
    ["user2@example.com"]="User123!"
    ["manager@example.com"]="Manager123!"
)

# Test rooms
rooms=(
    "Conference Room A"
    "Conference Room B"
    "Meeting Room 1"
    "Meeting Room 2"
    "Training Room"
)

# Register admin user first
echo "Registering admin user..."
curl -s -X POST "$API_BASE/auth/register" \
  -H "Content-Type: application/json" \
  -d '{
    "email": "admin@example.com",
    "password": "Admin123!",
    "name": "System Administrator"
  }' > /dev/null || echo "Admin user may already exist"

# Login as admin to get token
echo "Logging in as admin..."
token=$(curl -s -X POST "$API_BASE/auth/login" \
  -H "Content-Type: application/json" \
  -d '{
    "email": "admin@example.com",
    "password": "Admin123!"
  }' | jq -r '.token' 2>/dev/null || echo "")

if [ -z "$token" ] || [ "$token" = "null" ]; then
    echo "Failed to get admin token"
    exit 1
fi

echo "Admin token obtained successfully"

# Register other test users
for email in "${!users[@]}"; do
    if [ "$email" != "admin@example.com" ]; then
        echo "Registering user: $email"
        curl -s -X POST "$API_BASE/auth/register" \
          -H "Content-Type: application/json" \
          -d "{
            \"email\": \"$email\",
            \"password\": \"${users[$email]}\",
            \"name\": \"Test User\"
          }" > /dev/null || echo "User $email may already exist"
    fi
done

# Create test rooms
for i in "${!rooms[@]}"; do
    room_name="${rooms[$i]}"
    capacity=$((6 + i * 2))
    echo "Creating room: $room_name (capacity: $capacity)"
    
    curl -s -X POST "$API_BASE/rooms" \
      -H "Authorization: Bearer $token" \
      -H "Content-Type: application/json" \
      -d "{
        \"name\": \"$room_name\",
        \"capacity\": $capacity,
        \"equipment\": [\"Projector\", \"Whiteboard\"],
        \"isActive\": true
      }" > /dev/null || echo "Room '$room_name' may already exist"
done

echo "Test data seeding completed successfully!"