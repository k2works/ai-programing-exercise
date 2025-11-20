import { FastifyReply, FastifyRequest } from 'fastify';

declare module 'fastify' {
  interface FastifyInstance {
    authenticateApiKeyOrJwt: (request: FastifyRequest, reply: FastifyReply) => Promise<void>;
  }
}

/**
 * Verifies either a valid API key in `x-api-key` header or a valid JWT.
 * - APIキーは `process.env.API_KEY` と照合します（未設定ならAPIキーは無効、JWTのみ有効）。
 * - どちらか一方が有効であれば通過させます。
 * - どちらも満たさない場合は 401 を返します。
 */
export async function authenticateApiKeyOrJwt(request: FastifyRequest, reply: FastifyReply) {
  const providedKey = (request.headers['x-api-key'] || request.headers['X-API-KEY']) as string | undefined;
  const configuredKey = process.env.API_KEY;

  // 1) Check API Key first (if configured)
  if (configuredKey && providedKey && providedKey === configuredKey) {
    return; // Authorized by API key
  }

  // 2) Fallback to JWT
  try {
    await (request as any).jwtVerify();
    return; // Authorized by JWT
  } catch (err) {
    // Neither API key nor JWT is valid
    return reply.code(401).send({ error: 'Unauthorized' });
  }
}
