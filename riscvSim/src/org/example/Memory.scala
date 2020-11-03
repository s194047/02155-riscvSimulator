package org.example

class Memory(private val size: Int) {
    private val data = new Array[Byte](size)

    def loadMemory(newData: Array[Byte], address: Int): Unit = {
        newData.copyToArray(data, address)
    }

    def writeByte(address: Int, value: Int): Unit = {
        data(address) = value.toByte
    }

    def writeHalfword(address: Int, value: Int): Unit = {
        data(address) = (value & 0xff).toByte
        data(address + 1) = ((value >> 8) & 0xff).toByte
    }

    def writeWord(address: Int, value: Int): Unit = {
        data(address) = (value & 0xff).toByte
        data(address + 1) = ((value >> 8) & 0xff).toByte
        data(address + 2) = ((value >> 16) & 0xff).toByte
        data(address + 3) = ((value >> 24) & 0xff).toByte
    }

    def readByte(address: Int): Int = data(address)
    def readByteUnsigned(address: Int): Int = readByte(address) & 0xff

    def readHalfword(address: Int): Int = (readByte(address + 1) << 8) + readByteUnsigned(address)
    def readHalfwordUnsigned(address: Int): Int = (readByteUnsigned(address + 1) << 8) + readByteUnsigned(address)

    def readWord(address: Int): Int = (readHalfword(address + 2) << 16) + readHalfwordUnsigned(address)
}
