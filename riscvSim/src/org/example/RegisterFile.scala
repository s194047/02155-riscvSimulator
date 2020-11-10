package org.example

class RegisterFile(private val size: Int, private val offset: Int = 1) {
    private val data = new Array[Int](size - offset)

    def apply(register: Int): Int = if (register > 0) data(register - offset) else 0
    def update(register: Int, value: Int): Unit = {
        data(register - offset) = value
    }

    def printRegisters(): Unit = {
        for (i <- 0 until size - offset) {
            println(f"x${i + offset}%02d: ${data(i)}")
        }
    }


}
