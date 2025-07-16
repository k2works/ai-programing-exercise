<?php

namespace App;

class FizzBuzzListCommand implements FizzBuzzCommand
{
    use Assertions;
    
    private FizzBuzzType $type;
    private int $maxNumber;

    public function __construct(FizzBuzzType $type, int $maxNumber = 100)
    {
        $this->assert(fn() => $maxNumber <= 100, '最大値は100以下である必要があります');
        $this->type = $type;
        $this->maxNumber = $maxNumber;
    }

    public function execute(int $number = 0): FizzBuzzList
    {
        $values = [];
        for ($i = 1; $i <= $this->maxNumber; $i++) {
            $values[] = $this->type->generate($i);
        }

        return new FizzBuzzList($values);
    }
}
