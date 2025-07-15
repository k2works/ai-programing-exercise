import { FizzBuzz } from './fizzbuzz';

function main(): void {
  for (let i = 1; i <= 100; i++) {
    console.log(FizzBuzz.generate(i));
  }
}

main();
