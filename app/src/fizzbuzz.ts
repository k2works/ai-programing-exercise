export class FizzBuzz {
  static generate(n: number, type: number = 1): string {
    const isFizz = n % 3 === 0;
    const isBuzz = n % 5 === 0;

    switch (type) {
      case 1:
        if (isFizz && isBuzz) {
          return 'FizzBuzz';
        } else if (isFizz) {
          return 'Fizz';
        } else if (isBuzz) {
          return 'Buzz';
        }
        return n.toString();
      case 2:
        return n.toString();
      case 3:
        if (isFizz && isBuzz) {
          return 'FizzBuzz';
        }
        return n.toString();
      default:
        return n.toString();
    }
  }

  static generateRange(start: number, end: number): string[] {
    const result: string[] = [];
    for (let i = start; i <= end; i++) {
      result.push(this.generate(i));
    }
    return result;
  }

  static printRange(start: number, end: number): void {
    for (let i = start; i <= end; i++) {
      console.log(this.generate(i));
    }
  }
}
