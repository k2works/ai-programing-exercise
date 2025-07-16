<?php

namespace App;

class FizzBuzz
{
    public function generate(int $number, int $type = 1): string
    {
        if ($type === 2) {
            return (string) $number;
        }
        
        if ($type === 3) {
            if ($number % 3 === 0 && $number % 5 === 0) {
                return 'FizzBuzz';
            }
            return (string) $number;
        }
        
        if ($type !== 1) {
            throw new \InvalidArgumentException('タイプは1、2、3のいずれかを指定してください');
        }
        
        $result = (string) $number;
        if ($number % 3 === 0 && $number % 5 === 0) {
            $result = 'FizzBuzz';
        } elseif ($number % 3 === 0) {
            $result = 'Fizz';
        } elseif ($number % 5 === 0) {
            $result = 'Buzz';
        }

        return $result;
    }

    public function generateList(): array
    {
        $result = [];
        for ($i = 1; $i <= 100; $i++) {
            $result[] = $this->generate($i);
        }

        return $result;
    }

    public function printFizzBuzz(): void
    {
        $list = $this->generateList();
        foreach ($list as $item) {
            echo $item . "\n";
        }
    }
}
