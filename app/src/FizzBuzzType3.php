<?php

namespace App;

class FizzBuzzType3 implements FizzBuzzType
{
    public function generate(int $number): string
    {
        if ($number % 3 === 0 && $number % 5 === 0) {
            return 'FizzBuzz';
        }
        return (string) $number;
    }
}
