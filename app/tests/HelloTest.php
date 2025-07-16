<?php

namespace Tests;

use PHPUnit\Framework\TestCase;

class HelloTest extends TestCase
{
    public function testGreeting(): void
    {
        $this->assertEquals('hello world', $this->greeting());
    }

    private function greeting(): string
    {
        return 'hello world';
    }
}
