<?php

namespace App\Domain\Type;

use App\Domain\Model\FizzBuzzValue;

interface FizzBuzzType
{
    public function generate(int $number): FizzBuzzValue;
}
