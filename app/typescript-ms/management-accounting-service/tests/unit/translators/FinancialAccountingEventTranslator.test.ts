// tests/unit/translators/FinancialAccountingEventTranslator.test.ts

import { describe, it, expect, beforeEach } from 'vitest'
import { FinancialAccountingEventTranslator } from '../../../src/application/translators/FinancialAccountingEventTranslator'
import { JournalCreatedEventDto } from '../../../src/domain/models/external/FinancialAccountingEvent'

describe('FinancialAccountingEventTranslator', () => {
  let translator: FinancialAccountingEventTranslator

  beforeEach(() => {
    translator = new FinancialAccountingEventTranslator()
  })

  describe('translateToJournalCache', () => {
    it('should translate JournalCreatedEvent to JournalCache', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date('2024-01-15T10:00:00Z'),
        payload: {
          journalId: '123',
          fiscalYear: 2024,
          journalDate: '2024-01-15',
          totalDebitAmount: 10000,
          totalCreditAmount: 10000
        }
      }

      // Act
      const result = translator.translateToJournalCache(event)

      // Assert
      expect(result).toBeDefined()
      expect(result?.journalId).toBe('123')
      expect(result?.fiscalYear).toBe(2024)
      expect(result?.journalDate).toEqual(new Date('2024-01-15'))
      expect(result?.totalDebitAmount).toBe(10000)
      expect(result?.totalCreditAmount).toBe(10000)
      expect(result?.receivedAt).toBeInstanceOf(Date)
    })

    it('should handle journalDate as Date object', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '456',
          fiscalYear: 2024,
          journalDate: new Date('2024-02-20'),
          totalDebitAmount: 20000,
          totalCreditAmount: 20000
        }
      }

      // Act
      const result = translator.translateToJournalCache(event)

      // Assert
      expect(result?.journalDate).toEqual(new Date('2024-02-20'))
    })

    it('should return null for unsupported event types', () => {
      // Arrange
      const unsupportedEvent = {
        eventType: 'AccountCreated',
        occurredAt: new Date(),
        payload: {
          accountCode: '1010',
          accountName: '現金'
        }
      }

      // Act
      const result = translator.translateToJournalCache(unsupportedEvent as any)

      // Assert
      expect(result).toBeNull()
    })
  })

  describe('validateEvent', () => {
    it('should validate correct JournalCreatedEvent', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '123',
          fiscalYear: 2024,
          journalDate: new Date(),
          totalDebitAmount: 10000,
          totalCreditAmount: 10000
        }
      }

      // Act
      const validation = translator.validateEvent(event)

      // Assert
      expect(validation.valid).toBe(true)
      expect(validation.errors).toEqual([])
    })

    it('should detect missing eventType', () => {
      // Arrange
      const event = {
        occurredAt: new Date(),
        payload: {
          journalId: '123',
          fiscalYear: 2024,
          journalDate: new Date(),
          totalDebitAmount: 10000,
          totalCreditAmount: 10000
        }
      } as any

      // Act
      const validation = translator.validateEvent(event)

      // Assert
      expect(validation.valid).toBe(false)
      expect(validation.errors).toContain('eventType is required')
    })

    it('should detect missing journalId', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '',
          fiscalYear: 2024,
          journalDate: new Date(),
          totalDebitAmount: 10000,
          totalCreditAmount: 10000
        }
      }

      // Act
      const validation = translator.validateEvent(event)

      // Assert
      expect(validation.valid).toBe(false)
      expect(validation.errors).toContain('payload.journalId is required')
    })

    it('should detect invalid fiscalYear', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '123',
          fiscalYear: 0,
          journalDate: new Date(),
          totalDebitAmount: 10000,
          totalCreditAmount: 10000
        }
      }

      // Act
      const validation = translator.validateEvent(event)

      // Assert
      expect(validation.valid).toBe(false)
      expect(validation.errors).toContain('payload.fiscalYear must be a positive number')
    })

    it('should detect negative amounts', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '123',
          fiscalYear: 2024,
          journalDate: new Date(),
          totalDebitAmount: -10000,
          totalCreditAmount: -10000
        }
      }

      // Act
      const validation = translator.validateEvent(event)

      // Assert
      expect(validation.valid).toBe(false)
      expect(validation.errors).toContain('payload.totalDebitAmount must be non-negative')
      expect(validation.errors).toContain('payload.totalCreditAmount must be non-negative')
    })

    it('should detect debit-credit mismatch', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '123',
          fiscalYear: 2024,
          journalDate: new Date(),
          totalDebitAmount: 10000,
          totalCreditAmount: 15000
        }
      }

      // Act
      const validation = translator.validateEvent(event)

      // Assert
      expect(validation.valid).toBe(false)
      expect(validation.errors.some((e) => e.includes('Debit and credit amounts must match'))).toBe(true)
    })

    it('should allow small debit-credit differences (floating point tolerance)', () => {
      // Arrange
      const event: JournalCreatedEventDto = {
        eventType: 'JournalCreated',
        occurredAt: new Date(),
        payload: {
          journalId: '123',
          fiscalYear: 2024,
          journalDate: new Date(),
          totalDebitAmount: 10000.00,
          totalCreditAmount: 10000.005 // 0.005の差（許容範囲内）
        }
      }

      // Act
      const validation = translator.validateEvent(event)

      // Assert
      expect(validation.valid).toBe(true)
    })
  })
})
