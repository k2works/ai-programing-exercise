<?php

namespace App\Domain\Type;

use App\Domain\Model\FizzBuzzValue;

class FizzBuzzType1 implements FizzBuzzType
{
    public function generate(int $number): FizzBuzzValue
    {
        if ($number % 3 === 0 && $number % 5 === 0) {
            return new FizzBuzzValue($number, 'FizzBuzz');
        } elseif ($number % 3 === 0) {
            return new FizzBuzzValue($number, 'Fizz');
        } elseif ($number % 5 === 0) {
            return new FizzBuzzValue($number, 'Buzz');
        }

        return new FizzBuzzValue($number, (string) $number);
    }
}
