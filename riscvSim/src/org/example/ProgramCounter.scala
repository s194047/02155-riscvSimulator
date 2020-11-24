package org.example

class ProgramCounter(private var value: Int) {
    def apply(): Int = value
    def update(newValue: Int): Unit = {
        value = newValue
    }
    def +=(amount: Int): Unit = {
        value += amount
    }

    override def toString(): String = value.toString
}
