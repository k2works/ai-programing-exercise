import { FastifyError, FastifyReply, FastifyRequest } from 'fastify';
import { ZodError } from 'zod';
import {
  DomainException,
  ItemNotFoundException,
  DuplicateItemException,
  InsufficientInventoryException,
} from '../../domain/exception/DomainException';

/**
 * グローバルエラーハンドラー
 */
export async function errorHandler(
  error: FastifyError,
  request: FastifyRequest,
  reply: FastifyReply
) {
  // Zod バリデーションエラー
  if (error instanceof ZodError) {
    return reply.status(400).send({
      statusCode: 400,
      error: 'Bad Request',
      message: 'リクエストのバリデーションエラー',
      details: error.errors.map((e) => ({
        path: e.path.join('.'),
        message: e.message,
      })),
    });
  }

  // リソース未発見
  if (error instanceof ItemNotFoundException) {
    return reply.status(404).send({
      statusCode: 404,
      error: 'Not Found',
      message: error.message,
    });
  }

  // 重複エラー
  if (error instanceof DuplicateItemException) {
    return reply.status(409).send({
      statusCode: 409,
      error: 'Conflict',
      message: error.message,
    });
  }

  // 在庫不足
  if (error instanceof InsufficientInventoryException) {
    return reply.status(422).send({
      statusCode: 422,
      error: 'Unprocessable Entity',
      message: error.message,
    });
  }

  // その他のドメイン例外
  if (error instanceof DomainException) {
    return reply.status(422).send({
      statusCode: 422,
      error: 'Unprocessable Entity',
      message: error.message,
    });
  }

  // 予期しないエラー
  request.log.error(error, 'Unexpected error');
  return reply.status(500).send({
    statusCode: 500,
    error: 'Internal Server Error',
    message: 'サーバーエラーが発生しました',
  });
}
