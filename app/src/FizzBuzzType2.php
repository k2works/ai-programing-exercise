<?php

namespace App;

class FizzBuzzType2 implements FizzBuzzType
{
    public function generate(int $number): string
    {
        return (string) $number;
    }
}
