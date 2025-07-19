package application

import domain.model.FizzBuzzList
import domain.model.FizzBuzzValue
import domain.type.FizzBuzzType

class AssertionFailedException(message: String) : Exception(message)

object Assertions {
    fun require(condition: Boolean, message: String = "Assertion failed") {
        if (!condition) {
            throw AssertionFailedException(message)
        }
    }
}

interface FizzBuzzCommand {
    fun execute(): Any
}

class FizzBuzzValueCommand(private val type: FizzBuzzType) : FizzBuzzCommand {
    override fun execute(): Any {
        throw UnsupportedOperationException("Use execute(number) instead")
    }
    
    fun execute(number: Int): FizzBuzzValue {
        Assertions.require(number > 0, "値は正の値のみ許可する")
        val value = type.generate(number)
        return FizzBuzzValue(number, value)
    }
}

class FizzBuzzListCommand(private val type: FizzBuzzType) : FizzBuzzCommand {
    companion object {
        const val DEFAULT_END_NUMBER = 100
    }
    
    override fun execute(): FizzBuzzList {
        val values = (1..DEFAULT_END_NUMBER).map { number ->
            FizzBuzzValueCommand(type).execute(number).value
        }
        return FizzBuzzList(values)
    }
}