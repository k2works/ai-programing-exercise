<?php

namespace App;

class FizzBuzzValueCommand implements FizzBuzzCommand
{
    private FizzBuzzType $type;

    public function __construct(FizzBuzzType $type)
    {
        $this->type = $type;
    }

    public function execute(int $number): string
    {
        $value = $this->type->generate($number);
        return $value->getValue();
    }
}
