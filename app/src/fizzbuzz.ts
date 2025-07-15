export class FizzBuzz {
  static generate(n: number, type: number = 1): string {
    switch (type) {
      case 1:
        if (n % 3 === 0 && n % 5 === 0) {
          return 'FizzBuzz';
        } else if (n % 3 === 0) {
          return 'Fizz';
        } else if (n % 5 === 0) {
          return 'Buzz';
        }
        return n.toString();
      case 2:
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
