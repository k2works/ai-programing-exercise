<?php

namespace App\Domain\Model;

use App\Assertions;

class FizzBuzzValue
{
    use Assertions;
    
    private int $number;
    private string $value;

    public function __construct(int $number, string $value)
    {
        $this->assert(fn() => $number >= 0, '数値は0以上である必要があります');
        $this->number = $number;
        $this->value = $value;
    }

    public function getNumber(): int
    {
        return $this->number;
    }

    public function getValue(): string
    {
        return $this->value;
    }

    public function __toString(): string
    {
        return $this->number . ':' . $this->value;
    }

    public function equals(FizzBuzzValue $other): bool
    {
        return $this->number === $other->number && $this->value === $other->value;
    }
}
