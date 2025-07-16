<?php

namespace App;

class AssertionFailedException extends \InvalidArgumentException
{
    public function __construct(string $message = 'Assertion Failed')
    {
        parent::__construct($message);
    }
}

trait Assertions
{
    protected function assert(callable $condition, string $message = 'Assertion Failed'): void
    {
        if (!$condition()) {
            throw new AssertionFailedException($message);
        }
    }
}
