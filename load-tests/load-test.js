import http from 'k6/http';
import { check, sleep, group } from 'k6';
import { Counter, Trend, Rate } from 'k6/metrics';

// Custom metrics
const authSuccessRate = new Rate('auth_success_rate');
const reservationSuccessRate = new Rate('reservation_success_rate');
const responseTimeAuth = new Trend('response_time_auth');
const responseTimeReservation = new Trend('response_time_reservation');
const errorCounter = new Counter('errors');

// Test configuration
export const options = {
  scenarios: {
    // Quick load test: 0 to 20 users gradually to avoid rate limiting
    load_test: {
      executor: 'ramping-vus',
      startVUs: 1,
      stages: [
        { duration: '30s', target: 5 },   // Gentle ramp up to 5 users
        { duration: '1m', target: 10 },   // Ramp to 10 users
        { duration: '1m', target: 15 },   // Ramp to 15 users
        { duration: '2m', target: 20 },   // Ramp to 20 users and maintain
        { duration: '30s', target: 10 },  // Ramp down to 10
        { duration: '30s', target: 0 },   // Ramp down to 0
      ],
      gracefulRampDown: '30s',
    },
  },
  
  thresholds: {
    http_req_duration: ['p(95)<3000'], // 95% of requests under 3s (relaxed due to rate limiting)
    http_req_failed: ['rate<0.20'],    // Error rate under 20% (account for rate limiting)
    auth_success_rate: ['rate>0.70'],   // Auth success rate over 70%
    reservation_success_rate: ['rate>0.60'], // Reservation success rate over 60%
  },
};

// Configuration
const BASE_URL = __ENV.BASE_URL || 'http://localhost:5080';
const API_BASE = `${BASE_URL}/api`;

// Test data
const users = [
  { email: 'admin@example.com', password: 'Admin123!' },
  { email: 'user1@example.com', password: 'User123!' },
  { email: 'user2@example.com', password: 'User123!' },
  { email: 'manager@example.com', password: 'Manager123!' },
];

const rooms = [
  'Conference Room A',
  'Conference Room B', 
  'Meeting Room 1',
  'Meeting Room 2',
  'Training Room',
];

function getRandomUser() {
  return users[Math.floor(Math.random() * users.length)];
}

function getRandomRoom() {
  return rooms[Math.floor(Math.random() * rooms.length)];
}

function getRandomFutureDate() {
  const tomorrow = new Date();
  tomorrow.setDate(tomorrow.getDate() + 1);
  tomorrow.setHours(9 + Math.floor(Math.random() * 8)); // 9 AM to 5 PM
  tomorrow.setMinutes(0);
  tomorrow.setSeconds(0);
  tomorrow.setMilliseconds(0);
  return tomorrow.toISOString();
}

function login(user) {
  const loginPayload = {
    userId: user.email, // API expects userId, not email
    password: user.password,
  };

  const params = {
    headers: {
      'Content-Type': 'application/json',
    },
    tags: { name: 'login' },
  };

  const startTime = Date.now();
  const response = http.post(`${API_BASE}/auth/login`, JSON.stringify(loginPayload), params);
  const duration = Date.now() - startTime;
  
  responseTimeAuth.add(duration);
  
  const success = check(response, {
    'login status is 200': (r) => r.status === 200,
    'login response has token': (r) => {
      try {
        const body = JSON.parse(r.body);
        return body.token && body.token.length > 0;
      } catch (e) {
        return false;
      }
    },
  });

  authSuccessRate.add(success);
  
  if (!success) {
    errorCounter.add(1);
    console.error(`Login failed for ${user.email}: ${response.status} ${response.body}`);
    return null;
  }

  const body = JSON.parse(response.body);
  return body.token;
}

function getRooms(token) {
  const params = {
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json',
    },
    tags: { name: 'get_rooms' },
  };

  const response = http.get(`${API_BASE}/rooms`, params);
  
  check(response, {
    'get rooms status is 200': (r) => r.status === 200,
    'get rooms response is array': (r) => {
      try {
        const body = JSON.parse(r.body);
        return Array.isArray(body);
      } catch (e) {
        return false;
      }
    },
  });

  if (response.status !== 200) {
    errorCounter.add(1);
    return [];
  }

  return JSON.parse(response.body);
}

function createReservation(token, roomId) {
  const startTime = getRandomFutureDate();
  const endTime = new Date(startTime);
  endTime.setHours(endTime.getHours() + 2); // 2-hour reservation

  const reservationPayload = {
    roomId: roomId,
    startDateTime: startTime,
    endDateTime: endTime.toISOString(),
    purpose: `Load test reservation - ${Math.random().toString(36).substr(2, 9)}`,
  };

  const params = {
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json',
    },
    tags: { name: 'create_reservation' },
  };

  const requestStart = Date.now();
  const response = http.post(`${API_BASE}/reservations`, JSON.stringify(reservationPayload), params);
  const duration = Date.now() - requestStart;
  
  responseTimeReservation.add(duration);

  const success = check(response, {
    'create reservation status is 200 or 201': (r) => r.status === 200 || r.status === 201,
    'create reservation response has id': (r) => {
      try {
        const body = JSON.parse(r.body);
        return body.id && body.id > 0;
      } catch (e) {
        return false;
      }
    },
  });

  reservationSuccessRate.add(success);

  if (!success) {
    errorCounter.add(1);
    console.error(`Reservation creation failed: ${response.status} ${response.body}`);
    return null;
  }

  return JSON.parse(response.body);
}

function getReservations(token) {
  const params = {
    headers: {
      'Authorization': `Bearer ${token}`,
      'Content-Type': 'application/json',
    },
    tags: { name: 'get_reservations' },
  };

  const response = http.get(`${API_BASE}/reservations`, params);
  
  check(response, {
    'get reservations status is 200': (r) => r.status === 200,
    'get reservations response is array': (r) => {
      try {
        const body = JSON.parse(r.body);
        return Array.isArray(body);
      } catch (e) {
        return false;
      }
    },
  });

  if (response.status !== 200) {
    errorCounter.add(1);
  }

  return response.status === 200 ? JSON.parse(response.body) : [];
}

function healthCheck() {
  const response = http.get(`${BASE_URL}/health`);
  
  check(response, {
    'health check status is 200': (r) => r.status === 200,
  });

  if (response.status !== 200) {
    errorCounter.add(1);
  }
}

export default function () {
  // Health check
  group('Health Check', () => {
    healthCheck();
  });

  // User workflow
  group('User Authentication', () => {
    const user = getRandomUser();
    const token = login(user);
    
    if (!token) {
      return; // Skip the rest if login failed
    }

    // Get available rooms
    group('Room Management', () => {
      const rooms = getRooms(token);
      
      if (rooms.length > 0) {
        // Create a reservation
        group('Reservation Management', () => {
          const randomRoom = rooms[Math.floor(Math.random() * rooms.length)];
          const reservation = createReservation(token, randomRoom.id);
          
          if (reservation) {
            // Get user reservations
            getReservations(token);
            
            // Simulate some think time
            sleep(Math.random() * 3 + 2); // 2-5 seconds
          }
        });
      }
    });
  });

  // Simulate user think time between iterations
  sleep(Math.random() * 4 + 3); // 3-7 seconds to avoid rate limiting
}

export function handleSummary(data) {
  const summary = {
    testRun: {
      duration: data.state.testRunDurationMs,
      timestamp: new Date().toISOString(),
    },
    metrics: {
      http_reqs: data.metrics.http_reqs.values.count,
      http_req_duration_avg: data.metrics.http_req_duration.values.avg,
      http_req_duration_p95: data.metrics.http_req_duration.values['p(95)'],
      http_req_failed_rate: data.metrics.http_req_failed.values.rate,
      vus_max: data.metrics.vus_max.values.max,
    },
    customMetrics: {
      auth_success_rate: data.metrics.auth_success_rate?.values.rate || 0,
      reservation_success_rate: data.metrics.reservation_success_rate?.values.rate || 0,
      errors: data.metrics.errors?.values.count || 0,
    },
    thresholds: data.thresholds,
  };

  console.log('\n=== LOAD TEST SUMMARY ===');
  console.log(`Test Duration: ${summary.testRun.duration}ms`);
  console.log(`Total Requests: ${summary.metrics.http_reqs}`);
  console.log(`Average Response Time: ${summary.metrics.http_req_duration_avg.toFixed(2)}ms`);
  console.log(`95th Percentile Response Time: ${summary.metrics.http_req_duration_p95.toFixed(2)}ms`);
  console.log(`Error Rate: ${(summary.metrics.http_req_failed_rate * 100).toFixed(2)}%`);
  console.log(`Max Concurrent Users: ${summary.metrics.vus_max}`);
  console.log(`Auth Success Rate: ${(summary.customMetrics.auth_success_rate * 100).toFixed(2)}%`);
  console.log(`Reservation Success Rate: ${(summary.customMetrics.reservation_success_rate * 100).toFixed(2)}%`);
  console.log(`Total Errors: ${summary.customMetrics.errors}`);
  
  // Check if thresholds passed
  const thresholdsPassed = Object.values(data.thresholds).every(t => t.ok);
  console.log(`Thresholds: ${thresholdsPassed ? 'PASSED' : 'FAILED'}`);

  return {
    'stdout': '', // Required for k6 cloud
    'load-test-results.json': JSON.stringify(summary, null, 2),
  };
}