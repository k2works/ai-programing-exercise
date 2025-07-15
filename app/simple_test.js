const FizzBuzzListCommand = require('./lib/application/fizz_buzz_list_command');
const FizzBuzzType = require('./lib/domain/type/fizz_buzz_type');

try {
  console.log('Creating command...');
  const command = new FizzBuzzListCommand(FizzBuzzType.create(1));
  console.log('Executing command...');
  const result = command.execute(15);
  console.log('Results:');
  result.forEach((item, index) => {
    console.log(`${index + 1}: ${item.value}`);
  });
} catch (error) {
  console.error('Error:', error.message);
  console.error('Stack:', error.stack);
}
