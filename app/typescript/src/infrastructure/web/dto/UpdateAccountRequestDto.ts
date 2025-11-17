// src/infrastructure/web/dto/UpdateAccountRequestDto.ts
import { z } from 'zod'

/**
 * 勘定科目更新リクエストスキーマ
 */
export const UpdateAccountRequestSchema = z.object({
  accountName: z.string().min(1).max(40).optional(),
  accountKana: z.string().max(40).optional(),
  sumAccount: z.boolean().optional(),
  bsplDistinction: z.enum(['B', 'P']).optional(),
  transactionDistinction: z.enum(['1', '2', '3', '4', '5']).optional(),
  costDistinction: z.enum(['1', '2', '3']).optional(),
  displayOrder: z.number().int().nonnegative().optional(),
  aggregationTarget: z.boolean().optional()
})

export type UpdateAccountRequestDto = z.infer<typeof UpdateAccountRequestSchema>
