<?php

namespace App;

class FizzBuzz
{
    public const MAX_NUMBER = 100;
    private int $maxNumber;
    private FizzBuzzType $type;

    public function __construct(int $maxNumber = self::MAX_NUMBER, FizzBuzzType $type = null)
    {
        $this->maxNumber = $maxNumber;
        $this->type = $type ?? new FizzBuzzType1();
    }

    public function getMaxNumber(): int
    {
        return $this->maxNumber;
    }

    public function generate(int $number, int $type = 1): string
    {
        // 後方互換性のために type パラメータをサポート
        $typeObject = $this->getTypeObject($type);
        $value = $typeObject->generate($number);
        return $value->getValue();
    }

    private function getTypeObject(int $type): FizzBuzzType
    {
        switch ($type) {
            case 1:
                return new FizzBuzzType1();
            case 2:
                return new FizzBuzzType2();
            case 3:
                return new FizzBuzzType3();
            default:
                throw new \InvalidArgumentException('タイプは1、2、3のいずれかを指定してください');
        }
    }

    public function generateWithType(int $number): FizzBuzzValue
    {
        return $this->type->generate($number);
    }

    public function generateList(): array
    {
        $result = [];
        for ($i = 1; $i <= $this->maxNumber; $i++) {
            $value = $this->type->generate($i);
            $result[] = $value->getValue();
        }

        return $result;
    }

    public function generateValueList(): array
    {
        $result = [];
        for ($i = 1; $i <= $this->maxNumber; $i++) {
            $result[] = $this->type->generate($i);
        }

        return $result;
    }

    public function generateValueListAsCollection(): FizzBuzzList
    {
        $values = [];
        for ($i = 1; $i <= $this->maxNumber; $i++) {
            $values[] = $this->type->generate($i);
        }

        return new FizzBuzzList($values);
    }

    public function printFizzBuzz(): void
    {
        $list = $this->generateList();
        foreach ($list as $item) {
            echo $item . "\n";
        }
    }
}
