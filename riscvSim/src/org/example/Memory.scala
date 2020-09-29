package org.example

class Memory(val data: Array[Byte]) {
    def this(size: Int) = this(new Array[Byte](size))

    def writeByte(address: Int, value: Int): Unit = {
        data(address) = value.toByte
    }

    def writeHalfword(address: Int, value: Int): Unit = {
        data(address) = (value & 0xff).toByte
        data(address + 1) = ((value >> 8) & 0xffff).toByte
    }

    def writeWord(address: Int, value: Int): Unit = {
        data(address) = (value & 0xff).toByte
        data(address + 1) = ((value >> 8) & 0xff).toByte
        data(address + 2) = ((value >> 16) & 0xff).toByte
        data(address + 3) = ((value >> 24) & 0xff).toByte
    }

    def readByte(address: Int): Int = data(address)
    def readByteUnsigned(address: Int): Int = readByte(address) & 0xff

    def readHalfword(address: Int): Int = {
        val b0 = (data(address) & 0xff).toInt
        val b1 = data(address + 1).toInt

        (b0 << 0) + (b1 << 8)
    }
    def readHalfwordUnsigned(address: Int): Int = readHalfword(address) & 0xffff

    def readWord(address: Int): Int = {
        val b0 = (data(address) & 0xff).toInt
        val b1 = (data(address + 1) & 0xff).toInt
        val b2 = (data(address + 2) & 0xff).toInt
        val b3 = data(address + 3).toInt

        (b0 << 0) + (b1 << 8) + (b2 << 16) + (b3 << 24)
    }
}
