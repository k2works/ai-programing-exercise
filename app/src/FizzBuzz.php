<?php

namespace App;

class FizzBuzz
{
    public function generate(int $number): string
    {
        $result = (string) $number;
        if ($number % 3 === 0 && $number % 5 === 0) {
            $result = 'FizzBuzz';
        } elseif ($number % 3 === 0) {
            $result = 'Fizz';
        } elseif ($number % 5 === 0) {
            $result = 'Buzz';
        }
        return $result;
    }
}
