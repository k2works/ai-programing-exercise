<?php

namespace Tests;

use App\FizzBuzz;
use PHPUnit\Framework\TestCase;

class FizzBuzzTest extends TestCase
{
    private FizzBuzz $fizzbuzz;

    protected function setUp(): void
    {
        $this->fizzbuzz = new FizzBuzz();
    }

    // FizzBuzzのテスト
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

    // 配列とループ処理の学習用テスト
    public function test配列の繰り返し処理(): void
    {
        $values = [1, 2, 3];
        $result = [];
        foreach ($values as $value) {
            $result[] = $value * $value;
        }
        $this->assertEquals([1, 4, 9], $result);
    }

    public function testarray_filterで特定の条件を満たす要素だけを配列に入れて返す(): void
    {
        $values = [1.1, 2, 3.3, 4];
        $result = array_values(array_filter($values, 'is_int'));
        $this->assertEquals([2, 4], $result);
    }

    public function testarray_mapで新しい要素の配列を返す(): void
    {
        $fruits = ['apple', 'orange', 'pineapple', 'strawberry'];
        $result = array_map('strlen', $fruits);
        $this->assertEquals([5, 6, 9, 10], $result);
    }

    public function testarray_reduceで畳み込み演算を行う(): void
    {
        $values = [1, 2, 3, 4, 5];
        $result = array_reduce($values, function ($carry, $item) {
            return $carry + $item;
        }, 0);
        $this->assertEquals(15, $result);
    }

    public function test配列のソート(): void
    {
        $values = ['2', '4', '13', '3', '1', '10'];

        // 自然ソート（文字列として）
        $result1 = $values;
        sort($result1);
        $this->assertEquals(['1', '2', '3', '4', '10', '13'], $result1);

        // 数値としてソート（昇順）
        $result2 = $values;
        usort($result2, function ($a, $b) {
            return (int)$a <=> (int)$b;
        });
        $this->assertEquals(['1', '2', '3', '4', '10', '13'], $result2);

        // 数値としてソート（降順）
        $result3 = $values;
        usort($result3, function ($a, $b) {
            return (int)$b <=> (int)$a;
        });
        $this->assertEquals(['13', '10', '4', '3', '2', '1'], $result3);
    }

    public function test正規表現マッチング(): void
    {
        $fruits = ['apple', 'orange', 'pineapple', 'strawberry', 'apricot'];
        $result = array_values(preg_grep('/^a/', $fruits));
        $this->assertEquals(['apple', 'apricot'], $result);
    }

    public function testarray_sliceで配列の一部を取得(): void
    {
        $values = [1, 2, 3, 4, 5, 6, 7, 8, 9];

        // 先頭から5つ取得
        $result1 = array_slice($values, 0, 5);
        $this->assertEquals([1, 2, 3, 4, 5], $result1);

        // 5番目以降を取得
        $result2 = array_slice($values, 5);
        $this->assertEquals([6, 7, 8, 9], $result2);
    }
}
