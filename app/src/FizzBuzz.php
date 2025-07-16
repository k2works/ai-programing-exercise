<?php

namespace App;

class FizzBuzz
{
    public function generate(int $number): string
    {
        $result = (string) $number;
        if ($number % 3 === 0) {
            $result = 'Fizz';
        }
        return $result;
    }
}
