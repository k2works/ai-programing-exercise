<?php

namespace Tests;

use App\FizzBuzz;
use App\Domain\Type\FizzBuzzType1;
use App\Domain\Type\FizzBuzzType2;
use App\Domain\Type\FizzBuzzType3;
use App\Domain\Model\FizzBuzzValue;
use App\Domain\Model\FizzBuzzList;
use App\Application\FizzBuzzCommand;
use App\Application\FizzBuzzValueCommand;
use App\Application\FizzBuzzListCommand;
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

    // タイプごとに出力を切り替えることができる
    public function testタイプ1の場合_1を渡したら文字列1を返す(): void
    {
        $this->assertEquals('1', $this->fizzbuzz->generate(1, 1));
    }

    public function testタイプ2の場合_1を渡したら文字列1を返す(): void
    {
        $this->assertEquals('1', $this->fizzbuzz->generate(1, 2));
    }

    public function testタイプ2の場合_3を渡したら文字列3を返す(): void
    {
        $this->assertEquals('3', $this->fizzbuzz->generate(3, 2));
    }

    public function testタイプ2の場合_5を渡したら文字列5を返す(): void
    {
        $this->assertEquals('5', $this->fizzbuzz->generate(5, 2));
    }

    public function testタイプ2の場合_15を渡したら文字列15を返す(): void
    {
        $this->assertEquals('15', $this->fizzbuzz->generate(15, 2));
    }

    public function testタイプ3の場合_1を渡したら文字列1を返す(): void
    {
        $this->assertEquals('1', $this->fizzbuzz->generate(1, 3));
    }

    public function testタイプ3の場合_3を渡したら文字列3を返す(): void
    {
        $this->assertEquals('3', $this->fizzbuzz->generate(3, 3));
    }

    public function testタイプ3の場合_5を渡したら文字列5を返す(): void
    {
        $this->assertEquals('5', $this->fizzbuzz->generate(5, 3));
    }

    public function testタイプ3の場合_15を渡したら文字列FizzBuzzを返す(): void
    {
        $this->assertEquals('FizzBuzz', $this->fizzbuzz->generate(15, 3));
    }

    public function testそれ以外のタイプの場合_例外が発生する(): void
    {
        $this->expectException(\App\FizzBuzzTypeNotFoundException::class);
        $this->expectExceptionMessage('該当するタイプは存在しません: 4');
        $this->fizzbuzz->generate(1, 4);
    }

    // カプセル化のテスト
    public function testgetMaxNumberでプライベートフィールドの値を取得する(): void
    {
        $this->assertEquals(100, $this->fizzbuzz->getMaxNumber());
    }

    public function test最大値を指定してFizzBuzzの一覧を作成する(): void
    {
        $fizzbuzz = new FizzBuzz(15);
        $result = $fizzbuzz->generateList();
        $this->assertEquals(15, count($result));
        $this->assertEquals('FizzBuzz', $result[14]); // 15番目がFizzBuzz
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

    // ポリモーフィズムのテスト
    public function testType1オブジェクトでFizzBuzzを生成する(): void
    {
        $type1 = new FizzBuzzType1();
        $fizzbuzz = new FizzBuzz(100, $type1);
        
        $value1 = $fizzbuzz->generateWithType(1);
        $value3 = $fizzbuzz->generateWithType(3);
        $value5 = $fizzbuzz->generateWithType(5);
        $value15 = $fizzbuzz->generateWithType(15);
        
        $this->assertEquals('1', $value1->getValue());
        $this->assertEquals('Fizz', $value3->getValue());
        $this->assertEquals('Buzz', $value5->getValue());
        $this->assertEquals('FizzBuzz', $value15->getValue());
    }

    public function testType2オブジェクトで数値のみを生成する(): void
    {
        $type2 = new FizzBuzzType2();
        $fizzbuzz = new FizzBuzz(100, $type2);
        
        $value1 = $fizzbuzz->generateWithType(1);
        $value3 = $fizzbuzz->generateWithType(3);
        $value5 = $fizzbuzz->generateWithType(5);
        $value15 = $fizzbuzz->generateWithType(15);
        
        $this->assertEquals('1', $value1->getValue());
        $this->assertEquals('3', $value3->getValue());
        $this->assertEquals('5', $value5->getValue());
        $this->assertEquals('15', $value15->getValue());
    }

    public function testType3オブジェクトで15のときのみFizzBuzzを生成する(): void
    {
        $type3 = new FizzBuzzType3();
        $fizzbuzz = new FizzBuzz(100, $type3);
        
        $value1 = $fizzbuzz->generateWithType(1);
        $value3 = $fizzbuzz->generateWithType(3);
        $value5 = $fizzbuzz->generateWithType(5);
        $value15 = $fizzbuzz->generateWithType(15);
        
        $this->assertEquals('1', $value1->getValue());
        $this->assertEquals('3', $value3->getValue());
        $this->assertEquals('5', $value5->getValue());
        $this->assertEquals('FizzBuzz', $value15->getValue());
    }

    public function testGenerateListでタイプオブジェクトを使用する(): void
    {
        $type1 = new FizzBuzzType1();
        $fizzbuzz1 = new FizzBuzz(100, $type1);
        $result1 = $fizzbuzz1->generateList();
        $this->assertEquals('1', $result1[0]);
        $this->assertEquals('2', $result1[1]);
        $this->assertEquals('Fizz', $result1[2]);
        $this->assertEquals('4', $result1[3]);
        $this->assertEquals('Buzz', $result1[4]);
        $this->assertEquals('FizzBuzz', $result1[14]);
        $this->assertEquals('Buzz', $result1[99]);
        $this->assertEquals(100, count($result1));

        $type2 = new FizzBuzzType2();
        $fizzbuzz2 = new FizzBuzz(100, $type2);
        $result2 = $fizzbuzz2->generateList();
        $this->assertEquals('1', $result2[0]);
        $this->assertEquals('2', $result2[1]);
        $this->assertEquals('3', $result2[2]);
        $this->assertEquals('4', $result2[3]);
        $this->assertEquals('5', $result2[4]);
        $this->assertEquals('15', $result2[14]);
        $this->assertEquals('99', $result2[98]);
        $this->assertEquals(100, count($result2));

        $type3 = new FizzBuzzType3();
        $fizzbuzz3 = new FizzBuzz(100, $type3);
        $result3 = $fizzbuzz3->generateList();
        $this->assertEquals('1', $result3[0]);
        $this->assertEquals('2', $result3[1]);
        $this->assertEquals('3', $result3[2]);
        $this->assertEquals('4', $result3[3]);
        $this->assertEquals('5', $result3[4]);
        $this->assertEquals('FizzBuzz', $result3[14]);
        $this->assertEquals('100', $result3[99]);
        $this->assertEquals(100, count($result3));
    }

    // 値オブジェクトのテスト
    public function testFizzBuzzValueクラスで値オブジェクトを作成する(): void
    {
        $value = new \App\FizzBuzzValue(3, 'Fizz');
        
        $this->assertEquals(3, $value->getNumber());
        $this->assertEquals('Fizz', $value->getValue());
        $this->assertEquals('3:Fizz', (string) $value);
    }

    public function test値オブジェクトの等価性を確認する(): void
    {
        $value1 = new \App\FizzBuzzValue(3, 'Fizz');
        $value2 = new \App\FizzBuzzValue(3, 'Fizz');
        $value3 = new \App\FizzBuzzValue(5, 'Buzz');
        
        $this->assertTrue($value1->equals($value2));
        $this->assertFalse($value1->equals($value3));
    }

    public function test値オブジェクトを使ったFizzBuzz生成(): void
    {
        $type1 = new FizzBuzzType1();
        $fizzbuzz = new FizzBuzz(15, $type1);
        
        $valueList = $fizzbuzz->generateValueList();
        
        $this->assertEquals(3, $valueList[2]->getNumber());
        $this->assertEquals('Fizz', $valueList[2]->getValue());
        $this->assertEquals(15, $valueList[14]->getNumber());
        $this->assertEquals('FizzBuzz', $valueList[14]->getValue());
    }

    // ファーストクラスコレクションのテスト
    public function testFizzBuzzListクラスでコレクションを作成する(): void
    {
        $value1 = new \App\FizzBuzzValue(1, '1');
        $value2 = new \App\FizzBuzzValue(3, 'Fizz');
        $list = new \App\FizzBuzzList([$value1, $value2]);
        
        $this->assertEquals(2, $list->count());
        $this->assertEquals($value1, $list->get(0));
        $this->assertEquals($value2, $list->get(1));
    }

    public function testコレクションに要素を追加する(): void
    {
        $value1 = new \App\FizzBuzzValue(1, '1');
        $value2 = new \App\FizzBuzzValue(3, 'Fizz');
        $value3 = new \App\FizzBuzzValue(5, 'Buzz');
        
        $list1 = new \App\FizzBuzzList([$value1]);
        $list2 = $list1->add([$value2, $value3]);
        
        $this->assertEquals(1, $list1->count());
        $this->assertEquals(3, $list2->count());
        $this->assertEquals($value3, $list2->get(2));
    }

    public function testファーストクラスコレクションを使ったFizzBuzz生成(): void
    {
        $type1 = new FizzBuzzType1();
        $fizzbuzz = new FizzBuzz(15, $type1);
        
        $list = $fizzbuzz->generateValueListAsCollection();
        
        $this->assertEquals(15, $list->count());
        $this->assertEquals('Fizz', $list->get(2)->getValue());
        $this->assertEquals('Buzz', $list->get(4)->getValue());
        $this->assertEquals('FizzBuzz', $list->get(14)->getValue());
    }

    // Commandパターンのテスト
    public function testFizzBuzzValueCommandで値を生成する(): void
    {
        $type1 = new FizzBuzzType1();
        $command = new FizzBuzzValueCommand($type1);
        
        $this->assertEquals('1', $command->execute(1));
        $this->assertEquals('Fizz', $command->execute(3));
        $this->assertEquals('Buzz', $command->execute(5));
        $this->assertEquals('FizzBuzz', $command->execute(15));
    }

    public function testFizzBuzzListCommandでリストを生成する(): void
    {
        $type1 = new FizzBuzzType1();
        $command = new FizzBuzzListCommand($type1, 15);
        
        $list = $command->execute();
        
        $this->assertEquals(15, $list->count());
        $this->assertEquals('1', $list->get(0)->getValue());
        $this->assertEquals('Fizz', $list->get(2)->getValue());
        $this->assertEquals('Buzz', $list->get(4)->getValue());
        $this->assertEquals('FizzBuzz', $list->get(14)->getValue());
    }

    public function test異なるタイプでCommandパターンを使用する(): void
    {
        $type2 = new FizzBuzzType2();
        $valueCommand = new FizzBuzzValueCommand($type2);
        $listCommand = new FizzBuzzListCommand($type2, 5);
        
        $this->assertEquals('3', $valueCommand->execute(3));
        $this->assertEquals('15', $valueCommand->execute(15));
        
        $list = $listCommand->execute();
        $this->assertEquals('3', $list->get(2)->getValue());
        $this->assertEquals('5', $list->get(4)->getValue());
    }

    // デザインパターンのテスト
    public function testValueObjectパターンが適用されている(): void
    {
        $value = new \App\FizzBuzzValue(3, 'Fizz');
        
        // イミュータブルな値オブジェクト
        $this->assertEquals(3, $value->getNumber());
        $this->assertEquals('Fizz', $value->getValue());
        
        // 等価性チェック
        $value2 = new \App\FizzBuzzValue(3, 'Fizz');
        $this->assertTrue($value->equals($value2));
    }

    public function testStrategyパターンが適用されている(): void
    {
        // 異なる戦略（タイプ）を注入できる
        $context = new FizzBuzz(5, new FizzBuzzType1());
        $result1 = $context->generateValueList();
        
        $context2 = new FizzBuzz(5, new FizzBuzzType2());
        $result2 = $context2->generateValueList();
        
        $this->assertEquals('Fizz', $result1[2]->getValue());
        $this->assertEquals('3', $result2[2]->getValue());
    }

    public function testCommandパターンが適用されている(): void
    {
        // コマンドオブジェクトによる処理の実行
        $command1 = new FizzBuzzValueCommand(new FizzBuzzType1());
        $command2 = new FizzBuzzListCommand(new FizzBuzzType1(), 3);
        
        $this->assertEquals('Fizz', $command1->execute(3));
        
        $list = $command2->execute();
        $this->assertEquals(3, $list->count());
    }

    // 例外処理のテスト
    public function test値は正の値のみ許可する(): void
    {
        $this->expectException(\App\AssertionFailedException::class);
        $this->expectExceptionMessage('数値は0以上である必要があります');
        
        new \App\FizzBuzzValue(-1, 'invalid');
    }

    public function test最大値は100以下である必要がある(): void
    {
        $this->expectException(\App\AssertionFailedException::class);
        $this->expectExceptionMessage('最大値は100以下である必要があります');
        
        $type = new FizzBuzzType1();
        new FizzBuzzListCommand($type, 101);
    }

    public function testアサーションが正常に動作する(): void
    {
        // 正の値は許可される
        $value = new FizzBuzzValue(1, '1');
        $this->assertEquals(1, $value->getNumber());
        
        // 100以下は許可される
        $type = new FizzBuzzType1();
        $command = new FizzBuzzListCommand($type, 100);
        $list = $command->execute();
        $this->assertEquals(100, $list->count());
    }
}
