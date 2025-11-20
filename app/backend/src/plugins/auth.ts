import { FastifyRequest, FastifyReply } from 'fastify';

declare module 'fastify' {
  interface FastifyInstance {
    authenticate: (request: FastifyRequest, reply: FastifyReply) => Promise<void>;
  }
}

export async function authenticate(request: FastifyRequest, reply: FastifyReply) {
  try {
    console.log('Authorization header:', request.headers.authorization);
    await request.jwtVerify();
    console.log('JWT verified successfully:', request.user);
  } catch (err) {
    console.error('JWT verification failed:', err);
    reply.code(401).send({ error: 'Unauthorized' });
  }
}
