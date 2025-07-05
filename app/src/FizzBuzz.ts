export class FizzBuzz {
  private list: string[] = []
  private type: number

  constructor(type: number = 1) {
    if (type < 1 || type > 3) {
      throw new Error('該当するタイプは存在しません')
    }
    this.type = type
  }

  generate(number: number, type?: number): string {
    const typeToUse = type ?? this.type
    
    switch (typeToUse) {
      case 1:
        let result = number.toString()

        if (number % 3 === 0 && number % 5 === 0) {
          result = 'FizzBuzz'
        } else if (number % 3 === 0) {
          result = 'Fizz'
        } else if (number % 5 === 0) {
          result = 'Buzz'
        }

        return result
      case 2:
        return number.toString()
      case 3:
        if (number % 3 === 0 && number % 5 === 0) {
          return 'FizzBuzz'
        } else {
          return number.toString()
        }
      default:
        throw new Error('該当するタイプは存在しません')
    }
  }

  generateList(start: number = 1, end: number = 100): string[] {
    this.list = []
    for (let i = start; i <= end; i++) {
      this.list.push(this.generate(i))
    }
    return this.list
  }

  getList(): string[] {
    return this.list
  }
}
