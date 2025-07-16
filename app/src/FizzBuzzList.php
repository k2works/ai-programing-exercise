<?php

namespace App;

class FizzBuzzList
{
    private array $value;

    public function __construct(array $value)
    {
        $this->value = $value;
    }

    public function getValue(): array
    {
        return $this->value;
    }

    public function __toString(): string
    {
        return implode(',', array_map(function ($item) {
            return (string) $item;
        }, $this->value));
    }

    public function add(array $values): FizzBuzzList
    {
        return new FizzBuzzList(array_merge($this->value, $values));
    }

    public function count(): int
    {
        return count($this->value);
    }

    public function get(int $index): FizzBuzzValue
    {
        if (!isset($this->value[$index])) {
            throw new \OutOfBoundsException('Index out of bounds');
        }
        return $this->value[$index];
    }
}
