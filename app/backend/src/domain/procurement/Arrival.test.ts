import { describe, it, expect } from 'vitest';
import { Arrival, ArrivalLine } from './Arrival';

describe('Arrival', () => {
  describe('create', () => {
    it('should create a new arrival with uninspected status', () => {
      const lines = [new ArrivalLine(1, 1, 1, 100)];
      const arrival = Arrival.create(1, 1, new Date('2025-01-10'), lines, 'staff1');

      expect(arrival.getId()).toBe(1);
      expect(arrival.getInspectionStatus()).toBe('uninspected');
    });
  });

  describe('inspect', () => {
    it('should change status to inspected when all lines are accepted', () => {
      const lines = [new ArrivalLine(1, 1, 1, 100)];
      const arrival = Arrival.create(1, 1, new Date('2025-01-10'), lines, 'staff1');

      lines[0].inspect('accepted');
      arrival.inspect();

      expect(arrival.getInspectionStatus()).toBe('inspected');
    });

    it('should change status to returned when any line is rejected', () => {
      const lines = [new ArrivalLine(1, 1, 1, 100), new ArrivalLine(2, 2, 2, 50)];
      const arrival = Arrival.create(1, 1, new Date('2025-01-10'), lines, 'staff1');

      lines[0].inspect('accepted');
      lines[1].inspect('rejected');
      arrival.inspect();

      expect(arrival.getInspectionStatus()).toBe('returned');
    });

    it('should throw error if already inspected', () => {
      const lines = [new ArrivalLine(1, 1, 1, 100)];
      const arrival = Arrival.reconstruct(
        1,
        1,
        new Date('2025-01-10'),
        lines,
        'inspected',
        'staff1',
        new Date(),
        new Date()
      );

      expect(() => arrival.inspect()).toThrow('既に検収済みです');
    });

    it('should throw error if not all lines are inspected', () => {
      const lines = [new ArrivalLine(1, 1, 1, 100)];
      const arrival = Arrival.create(1, 1, new Date('2025-01-10'), lines, 'staff1');

      expect(() => arrival.inspect()).toThrow('全ての明細を検収してください');
    });
  });

  describe('ArrivalLine', () => {
    it('should inspect line', () => {
      const line = new ArrivalLine(1, 1, 1, 100);

      line.inspect('accepted');

      expect(line.inspectionResult).toBe('accepted');
    });
  });
});
