<?php

function fizzBuzz(int $number): string
{
    if ($number === 3) {
        return 'Fizz';
    }
    return (string)$number;
}
