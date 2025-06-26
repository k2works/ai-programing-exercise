<?php

require_once __DIR__ . '/../src/fizz_buzz.php';

use PHPUnit\Framework\TestCase;

class FizzBuzzTest extends TestCase
{
    public function test_1を渡したら文字列1を返す(): void
    {
        $this->assertSame('1', fizzBuzz(1));
    }

    public function test_2を渡したら文字列2を返す(): void
    {
        $this->assertSame('2', fizzBuzz(2));
    }

    public function test_3を渡したら文字列Fizzを返す(): void
    {
        $this->assertSame('Fizz', fizzBuzz(3));
    }

    public function test_5を渡したら文字列Buzzを返す(): void
    {
        $this->assertSame('Buzz', fizzBuzz(5));
    }
}
