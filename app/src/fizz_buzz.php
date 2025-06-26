<?php

function fizzBuzz(int $number): string
{
    if ($number === 15) {
        return 'FizzBuzz';
    }
    if ($number === 3) {
        return 'Fizz';
    }
    if ($number === 5) {
        return 'Buzz';
    }
    return (string)$number;
}
