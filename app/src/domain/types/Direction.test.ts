import { describe, it, expect } from 'vitest';
import {
  type Direction,
  type Rotation,
  DIRECTIONS,
  ROTATIONS,
  isValidDirection,
  isValidRotation,
  getNextRotation,
  getPreviousRotation,
} from './Direction';

describe('Direction', () => {
  describe('DIRECTIONS', () => {
    it('3つの方向が定義されている', () => {
      expect(DIRECTIONS).toHaveLength(3);
      expect(DIRECTIONS).toEqual(['left', 'right', 'down']);
    });
  });

  describe('ROTATIONS', () => {
    it('4つの回転角度が定義されている', () => {
      expect(ROTATIONS).toHaveLength(4);
      expect(ROTATIONS).toEqual([0, 90, 180, 270]);
    });
  });

  describe('isValidDirection', () => {
    it('有効な方向の場合trueを返す', () => {
      const validDirections: Direction[] = ['left', 'right', 'down'];
      
      validDirections.forEach(direction => {
        expect(isValidDirection(direction)).toBe(true);
      });
    });

    it('無効な方向の場合falseを返す', () => {
      const invalidDirections = ['up', 'forward', 'back', '', 'LEFT'];
      
      invalidDirections.forEach(direction => {
        expect(isValidDirection(direction)).toBe(false);
      });
    });
  });

  describe('isValidRotation', () => {
    it('有効な回転角度の場合trueを返す', () => {
      const validRotations: Rotation[] = [0, 90, 180, 270];
      
      validRotations.forEach(rotation => {
        expect(isValidRotation(rotation)).toBe(true);
      });
    });

    it('無効な回転角度の場合falseを返す', () => {
      const invalidRotations = [45, 135, 225, 315, 360, -90];
      
      invalidRotations.forEach(rotation => {
        expect(isValidRotation(rotation)).toBe(false);
      });
    });
  });

  describe('getNextRotation', () => {
    it('時計回りに90度回転した角度を返す', () => {
      expect(getNextRotation(0)).toBe(90);
      expect(getNextRotation(90)).toBe(180);
      expect(getNextRotation(180)).toBe(270);
      expect(getNextRotation(270)).toBe(0);
    });
  });

  describe('getPreviousRotation', () => {
    it('反時計回りに90度回転した角度を返す', () => {
      expect(getPreviousRotation(0)).toBe(270);
      expect(getPreviousRotation(90)).toBe(0);
      expect(getPreviousRotation(180)).toBe(90);
      expect(getPreviousRotation(270)).toBe(180);
    });
  });

  describe('回転の循環性', () => {
    it('4回時計回りに回転すると元の角度に戻る', () => {
      let rotation: Rotation = 0;
      
      rotation = getNextRotation(rotation);
      rotation = getNextRotation(rotation);
      rotation = getNextRotation(rotation);
      rotation = getNextRotation(rotation);
      
      expect(rotation).toBe(0);
    });

    it('4回反時計回りに回転すると元の角度に戻る', () => {
      let rotation: Rotation = 0;
      
      rotation = getPreviousRotation(rotation);
      rotation = getPreviousRotation(rotation);
      rotation = getPreviousRotation(rotation);
      rotation = getPreviousRotation(rotation);
      
      expect(rotation).toBe(0);
    });
  });
});