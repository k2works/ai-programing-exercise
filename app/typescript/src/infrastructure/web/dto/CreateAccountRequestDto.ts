// src/infrastructure/web/dto/CreateAccountRequestDto.ts
import { z } from 'zod'

/**
 * 勘定科目作成リクエストスキーマ
 */
export const CreateAccountRequestSchema = z.object({
  accountCode: z.string().min(1).max(10),
  accountName: z.string().min(1).max(40),
  accountKana: z.string().max(40).optional(),
  accountType: z.enum(['資産', '負債', '純資産', '収益', '費用']),
  sumAccount: z.boolean().optional().default(false),
  bsplDistinction: z.enum(['B', 'P']).optional(),
  transactionDistinction: z.enum(['1', '2', '3', '4', '5']).optional(),
  costDistinction: z.enum(['1', '2', '3']).optional(),
  displayOrder: z.number().int().nonnegative().optional(),
  aggregationTarget: z.boolean().optional().default(true)
})

export type CreateAccountRequestDto = z.infer<typeof CreateAccountRequestSchema>
