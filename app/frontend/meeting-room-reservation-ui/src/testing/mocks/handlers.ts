import { http, HttpResponse } from 'msw'

export const handlers = [
  // Auth endpoints
  http.post('/api/auth/login', () => {
    return HttpResponse.json({
      user: { id: '1', name: 'Test User', email: 'test@example.com', role: 'USER' },
      token: 'mock-jwt-token',
    })
  }),

  // Reservations endpoints
  http.get('/api/reservations', () => {
    return HttpResponse.json([
      {
        id: '1',
        roomName: 'Room A',
        startTime: '2024-09-12T10:00:00Z',
        endTime: '2024-09-12T11:00:00Z',
        purpose: 'Team Meeting',
        status: 'CONFIRMED',
      },
      {
        id: '2',
        roomName: 'Room B',
        startTime: '2024-09-12T14:00:00Z',
        endTime: '2024-09-12T15:00:00Z',
        purpose: 'Client Call',
        status: 'CONFIRMED',
      },
    ])
  }),

  http.post('/api/reservations', () => {
    return HttpResponse.json(
      {
        id: '3',
        roomName: 'Room C',
        startTime: '2024-09-12T16:00:00Z',
        endTime: '2024-09-12T17:00:00Z',
        purpose: 'New Meeting',
        status: 'CONFIRMED',
      },
      { status: 201 }
    )
  }),

  http.delete('/api/reservations/:id', () => {
    return new HttpResponse(null, { status: 204 })
  }),
]
