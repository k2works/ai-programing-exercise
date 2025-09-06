# Seed test data for load testing
$baseUrl = "http://localhost:5080"

# Test users
$users = @(
    @{ email = "admin@example.com"; password = "Admin123!"; role = "Admin" },
    @{ email = "user1@example.com"; password = "User123!"; role = "User" },
    @{ email = "user2@example.com"; password = "User123!"; role = "User" },
    @{ email = "manager@example.com"; password = "Manager123!"; role = "Manager" }
)

# Test rooms
$rooms = @(
    @{ name = "Conference Room A"; capacity = 10; equipment = @("Projector", "Whiteboard") },
    @{ name = "Conference Room B"; capacity = 8; equipment = @("Projector") },
    @{ name = "Meeting Room 1"; capacity = 6; equipment = @("TV Screen") },
    @{ name = "Meeting Room 2"; capacity = 4; equipment = @() },
    @{ name = "Training Room"; capacity = 20; equipment = @("Projector", "Whiteboard", "Audio System") }
)

Write-Host "Seeding test data..." -ForegroundColor Green

# Register admin user first
$adminBody = @{
    email = $users[0].email
    password = $users[0].password
    name = "System Administrator"
} | ConvertTo-Json -Depth 2

try {
    $adminResponse = Invoke-RestMethod -Uri "$baseUrl/api/auth/register" -Method POST -Body $adminBody -ContentType "application/json"
    Write-Host "Admin user registered successfully" -ForegroundColor Green
} catch {
    Write-Host "Admin user may already exist or registration failed: $($_.Exception.Message)" -ForegroundColor Yellow
}

# Login as admin to get token
$loginBody = @{
    email = $users[0].email
    password = $users[0].password
} | ConvertTo-Json

try {
    $loginResponse = Invoke-RestMethod -Uri "$baseUrl/api/auth/login" -Method POST -Body $loginBody -ContentType "application/json"
    $token = $loginResponse.token
    Write-Host "Admin login successful" -ForegroundColor Green
} catch {
    Write-Host "Admin login failed: $($_.Exception.Message)" -ForegroundColor Red
    exit 1
}

$headers = @{
    "Authorization" = "Bearer $token"
    "Content-Type" = "application/json"
}

# Register other test users
for ($i = 1; $i -lt $users.Count; $i++) {
    $userBody = @{
        email = $users[$i].email
        password = $users[$i].password
        name = "Test User $i"
    } | ConvertTo-Json -Depth 2

    try {
        $userResponse = Invoke-RestMethod -Uri "$baseUrl/api/auth/register" -Method POST -Body $userBody -ContentType "application/json"
        Write-Host "User $($users[$i].email) registered successfully" -ForegroundColor Green
    } catch {
        Write-Host "User $($users[$i].email) may already exist: $($_.Exception.Message)" -ForegroundColor Yellow
    }
}

# Create test rooms
foreach ($room in $rooms) {
    $roomBody = @{
        name = $room.name
        capacity = $room.capacity
        equipment = $room.equipment
        isActive = $true
    } | ConvertTo-Json -Depth 2

    try {
        $roomResponse = Invoke-RestMethod -Uri "$baseUrl/api/rooms" -Method POST -Body $roomBody -ContentType "application/json" -Headers $headers
        Write-Host "Room '$($room.name)' created successfully" -ForegroundColor Green
    } catch {
        Write-Host "Room '$($room.name)' may already exist: $($_.Exception.Message)" -ForegroundColor Yellow
    }
}

Write-Host "Test data seeding completed!" -ForegroundColor Green