package org.example

object Simulator {
    def main(args: Array[String]): Unit = {
        val memory = new Memory(32)

        memory.writeByte(0, 0xff)
        memory.writeHalfword(4, 0xdead)
        memory.writeWord(8, 0xdeadbeef)

        println(memory.readByte(0).toHexString)
        println(memory.readByteUnsigned(0).toHexString)
        println(memory.readHalfword(4).toHexString)
        println(memory.readHalfwordUnsigned(4).toHexString)
        println(memory.readWord(8).toHexString)
    }
}
