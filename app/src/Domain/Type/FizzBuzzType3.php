<?php

namespace App\Domain\Type;

use App\Domain\Model\FizzBuzzValue;

class FizzBuzzType3 implements FizzBuzzType
{
    public function generate(int $number): FizzBuzzValue
    {
        if ($number % 3 === 0 && $number % 5 === 0) {
            return new FizzBuzzValue($number, 'FizzBuzz');
        }
        return new FizzBuzzValue($number, (string) $number);
    }
}
