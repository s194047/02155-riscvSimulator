package org.example

import java.io._
import scala.language.postfixOps

object Simulator {
    def main(args: Array[String]): Unit = {

        // Read from file into Byte Array.
        // val bis = new BufferedInputStream(new FileInputStream("riscvSim/resources/test1.bin"))
        // val byteArray = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray

        // Choose first argument as memory size and 1024 as default.
        val memorySize = if (args.length > 0) {
            args(0).toIntOption.getOrElse(0)
        } else 1024

        val memory = new Memory(memorySize)

        // Test reads and writes
        memory.writeByte(0, 0xef)
        memory.writeByte(1, 0xbe)
        memory.writeByte(2, 0xad)
        memory.writeByte(3, 0xde)
        memory.writeHalfword(4, 0xdead)
        memory.writeWord(8, 0xdeadbeef)

        println(memory.readByte(0).toHexString)
        println(memory.readByteUnsigned(0).toHexString)
        println(memory.readHalfword(0).toHexString)
        println(memory.readHalfwordUnsigned(4).toHexString)
        println(memory.readWord(0).toHexString)
    }
}
