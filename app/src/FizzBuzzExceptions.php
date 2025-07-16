<?php

namespace App;

class FizzBuzzTypeNotFoundException extends \InvalidArgumentException
{
    public function __construct(int $type)
    {
        parent::__construct("該当するタイプは存在しません: {$type}");
    }
}

class InvalidFizzBuzzValueException extends \InvalidArgumentException
{
    public function __construct(int $number)
    {
        parent::__construct("無効な値です: {$number}");
    }
}

class InvalidMaxNumberException extends \InvalidArgumentException
{
    public function __construct(int $maxNumber)
    {
        parent::__construct("無効な最大値です: {$maxNumber}");
    }
}
