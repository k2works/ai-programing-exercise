<?php

namespace Tests;

use PHPUnit\Framework\TestCase;
use App\FizzBuzz;

class FizzBuzzTest extends TestCase
{
    private FizzBuzz $fizzbuzz;

    protected function setUp(): void
    {
        $this->fizzbuzz = new FizzBuzz();
    }

    public function test1を渡したら文字列1を返す(): void
    {
        $this->assertEquals('1', $this->fizzbuzz->generate(1));
    }

    public function test2を渡したら文字列2を返す(): void
    {
        $this->assertEquals('2', $this->fizzbuzz->generate(2));
    }
}
