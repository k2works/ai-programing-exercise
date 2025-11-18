// src/api/routes/account.routes.ts
import { FastifyInstance } from 'fastify'
import { PrismaClient } from '@prisma/client'
import { prisma as defaultPrisma } from '../lib/prisma'
import { AccountMapper } from '../../infrastructure/persistence/mapper/AccountMapper'
import { AccountPersistenceAdapter } from '../../infrastructure/persistence/AccountPersistenceAdapter'
import { AccountService } from '../../application/port/in/AccountService'
import { AccountController } from '../../infrastructure/web/controller/AccountController'
import { CreateAccountRequestDto } from '../../infrastructure/web/dto/CreateAccountRequestDto'
import { UpdateAccountRequestDto } from '../../infrastructure/web/dto/UpdateAccountRequestDto'

/**
 * 勘定科目 API ルート
 * 依存関係の配線と Fastify へのルート登録
 *
 * @param app - Fastify インスタンス
 * @param options - オプション（テスト時に Prisma インスタンスをオーバーライド可能）
 */
export async function accountRoutes(
  app: FastifyInstance,
  options?: { prisma?: PrismaClient }
): Promise<void> {
  // 依存関係の構築（Dependency Injection）
  const prisma = options?.prisma || defaultPrisma
  const accountMapper = new AccountMapper()
  const accountRepository = new AccountPersistenceAdapter(prisma, accountMapper)
  const accountService = new AccountService(accountRepository)
  const accountController = new AccountController(accountService)

  /**
   * POST /accounts - 勘定科目を作成
   */
  app.post<{ Body: CreateAccountRequestDto }>(
    '/accounts',
    {
      schema: {
        tags: ['accounts'],
        summary: '勘定科目を作成',
        description: '新しい勘定科目を作成します',
        response: {
          201: {
            description: '作成成功',
            type: 'object',
            properties: {
              success: { type: 'boolean' },
              data: {
                type: 'object',
                properties: {
                  account: {
                    type: 'object',
                    properties: {
                      accountCode: { type: 'string' },
                      accountName: { type: 'string' },
                      accountKana: { type: 'string' },
                      accountType: { type: 'string' },
                      sumAccount: { type: 'boolean' },
                      bsplDistinction: { type: 'string' },
                      transactionDistinction: { type: 'string' },
                      costDistinction: { type: 'string' },
                      displayOrder: { type: 'number' },
                      aggregationTarget: { type: 'boolean' }
                    }
                  }
                }
              }
            }
          },
          400: {
            description: 'バリデーションエラー',
            type: 'object',
            properties: {
              success: { type: 'boolean' },
              error: { type: 'string' },
              code: { type: 'string' }
            }
          },
          409: {
            description: '科目コード重複',
            type: 'object',
            properties: {
              success: { type: 'boolean' },
              error: { type: 'string' },
              code: { type: 'string' }
            }
          }
        }
      }
    },
    async (request, reply) => {
      await accountController.createAccount(request, reply)
    }
  )

  /**
   * GET /accounts - 全勘定科目を取得
   */
  app.get(
    '/accounts',
    {
      schema: {
        tags: ['accounts'],
        summary: '全勘定科目を取得',
        description: '登録されている全ての勘定科目を取得します',
        response: {
          200: {
            description: '取得成功',
            type: 'object',
            properties: {
              success: { type: 'boolean' },
              data: {
                type: 'object',
                properties: {
                  accounts: {
                    type: 'array',
                    items: {
                      type: 'object',
                      properties: {
                        accountCode: { type: 'string' },
                        accountName: { type: 'string' },
                        accountType: { type: 'string' }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    },
    async (request, reply) => {
      await accountController.getAllAccounts(request, reply)
    }
  )

  /**
   * GET /accounts/:code - 科目コードで勘定科目を取得
   */
  app.get<{ Params: { code: string } }>(
    '/accounts/:code',
    {
      schema: {
        tags: ['accounts'],
        summary: '科目コードで勘定科目を取得',
        description: '指定された科目コードの勘定科目を取得します',
        params: {
          type: 'object',
          properties: {
            code: { type: 'string', description: '科目コード' }
          },
          required: ['code']
        }
      }
    },
    async (request, reply) => {
      await accountController.getAccountByCode(request, reply)
    }
  )

  /**
   * GET /accounts/bspl/:distinction - BSPL区分で勘定科目を取得
   */
  app.get<{ Params: { distinction: string } }>(
    '/accounts/bspl/:distinction',
    {
      schema: {
        tags: ['accounts'],
        summary: 'BSPL区分で勘定科目を取得',
        description: 'B（貸借対照表）または P（損益計算書）で勘定科目を取得します',
        params: {
          type: 'object',
          properties: {
            distinction: {
              type: 'string',
              enum: ['B', 'P'],
              description: 'BSPL区分（B=貸借対照表、P=損益計算書）'
            }
          },
          required: ['distinction']
        },
        response: {
          200: {
            description: '取得成功',
            type: 'object',
            properties: {
              success: { type: 'boolean' },
              data: {
                type: 'object',
                properties: {
                  accounts: { type: 'array' }
                }
              }
            }
          }
        }
      }
    },
    async (request, reply) => {
      await accountController.getAccountsByBspl(request, reply)
    }
  )

  /**
   * PUT /accounts/:code - 勘定科目を更新
   */
  app.put<{ Params: { code: string }; Body: UpdateAccountRequestDto }>(
    '/accounts/:code',
    {
      schema: {
        tags: ['accounts'],
        summary: '勘定科目を更新',
        description: '指定された科目コードの勘定科目を更新します',
        params: {
          type: 'object',
          properties: {
            code: { type: 'string', description: '科目コード' }
          },
          required: ['code']
        }
        // response schema removed - it was stripping properties
      }
    },
    async (request, reply) => {
      await accountController.updateAccount(request, reply)
    }
  )

  /**
   * DELETE /accounts/:code - 勘定科目を削除
   */
  app.delete<{ Params: { code: string } }>(
    '/accounts/:code',
    {
      schema: {
        tags: ['accounts'],
        summary: '勘定科目を削除',
        description: '指定された科目コードの勘定科目を削除します',
        params: {
          type: 'object',
          properties: {
            code: { type: 'string', description: '科目コード' }
          },
          required: ['code']
        },
        response: {
          204: {
            description: '削除成功',
            type: 'null'
          },
          404: {
            description: '科目が見つかりません',
            type: 'object',
            properties: {
              success: { type: 'boolean' },
              error: { type: 'string' },
              code: { type: 'string' }
            }
          }
        }
      }
    },
    async (request, reply) => {
      await accountController.deleteAccount(request, reply)
    }
  )
}
