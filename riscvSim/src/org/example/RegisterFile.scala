package org.example

class RegisterFile(private val size: Int, private val offset: Int = 1) {
    private val data = new Array[Int](size - offset)

    def readRegister(register: Int): Int = data(register - offset)
    def writeRegister(register: Int, value: Int): Unit = {
        data(register - offset) = value
    }
}
