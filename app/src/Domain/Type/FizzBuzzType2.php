<?php

namespace App\Domain\Type;

use App\Domain\Model\FizzBuzzValue;

class FizzBuzzType2 implements FizzBuzzType
{
    public function generate(int $number): FizzBuzzValue
    {
        return new FizzBuzzValue($number, (string) $number);
    }
}
