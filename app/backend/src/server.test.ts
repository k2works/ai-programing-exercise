import { describe, it, expect, afterAll } from 'vitest';
import { createServer } from './server.js';

describe('Server', () => {
  it('should create server successfully', async () => {
    const server = await createServer();
    expect(server).toBeDefined();
    await server.close();
  });

  it('should respond to health check', async () => {
    const server = await createServer();
    const response = await server.inject({
      method: 'GET',
      url: '/health',
    });
    
    expect(response.statusCode).toBe(200);
    expect(JSON.parse(response.body)).toEqual({ status: 'ok' });
    
    await server.close();
  });

  it('should have Swagger documentation', async () => {
    const server = await createServer();
    const response = await server.inject({
      method: 'GET',
      url: '/docs',
    });
    
    expect(response.statusCode).toBe(200);
    
    await server.close();
  });
});
