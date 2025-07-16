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

    public function test3を渡したら文字列Fizzを返す(): void
    {
        $this->assertEquals('Fizz', $this->fizzbuzz->generate(3));
    }

    public function test5を渡したら文字列Buzzを返す(): void
    {
        $this->assertEquals('Buzz', $this->fizzbuzz->generate(5));
    }

    public function test15を渡したら文字列FizzBuzzを返す(): void
    {
        $this->assertEquals('FizzBuzz', $this->fizzbuzz->generate(15));
    }

    public function test1から100までのFizzBuzz配列を返す(): void
    {
        $result = $this->fizzbuzz->generateList();
        $this->assertEquals('1', $result[0]);
        $this->assertEquals('2', $result[1]);
        $this->assertEquals('Fizz', $result[2]);
        $this->assertEquals('4', $result[3]);
        $this->assertEquals('Buzz', $result[4]);
        $this->assertEquals('FizzBuzz', $result[14]);
        $this->assertEquals('Buzz', $result[99]);
        $this->assertEquals(100, count($result));
    }

    public function testFizzBuzzの結果を標準出力に出力する(): void
    {
        $this->expectOutputString("1\n2\nFizz\n4\nBuzz\nFizz\n7\n8\nFizz\nBuzz\n11\nFizz\n13\n14\nFizzBuzz\n16\n17\nFizz\n19\nBuzz\nFizz\n22\n23\nFizz\nBuzz\n26\nFizz\n28\n29\nFizzBuzz\n31\n32\nFizz\n34\nBuzz\nFizz\n37\n38\nFizz\nBuzz\n41\nFizz\n43\n44\nFizzBuzz\n46\n47\nFizz\n49\nBuzz\nFizz\n52\n53\nFizz\nBuzz\n56\nFizz\n58\n59\nFizzBuzz\n61\n62\nFizz\n64\nBuzz\nFizz\n67\n68\nFizz\nBuzz\n71\nFizz\n73\n74\nFizzBuzz\n76\n77\nFizz\n79\nBuzz\nFizz\n82\n83\nFizz\nBuzz\n86\nFizz\n88\n89\nFizzBuzz\n91\n92\nFizz\n94\nBuzz\nFizz\n97\n98\nFizz\nBuzz\n");
        $this->fizzbuzz->printFizzBuzz();
    }
}
