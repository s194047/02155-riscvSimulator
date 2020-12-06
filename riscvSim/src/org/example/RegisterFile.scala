package org.example

import java.io._

class RegisterFile(private val size: Int, private val offset: Int = 0, private val verbose: Boolean = true, private val dumpOutputFilePathOpt: Option[String]) {
    private val data = new Array[Int](size - offset)

    def apply(register: Int): Int = if (register > 0) data(register - offset) else 0
    def update(register: Int, value: Int): Unit = {
        if (register - offset >= 0) {
            data(register - offset) = value
        }
    }

    def printRegisters(inline: Boolean = false): Unit = {
        if (inline) {
            println("Register contents:")
            for (i <- 0 until size) {
                print(f"x${i}%02d: ${this(i)}" + (if (i == size - 1) "" else " "))
            }
            println
        } else if (verbose) {
            println("Register contents:")
            for (i <- 0 until size) {
                println(f"x${i}%02d: ${this(i)}%11d 0x${this(i)}%08x")
            }
        }
    }

    def writeRegisterDump: Unit = {
        for (dumpOutputFilePath <- dumpOutputFilePathOpt) {
            println(s"Dumping registers to '$dumpOutputFilePath'.")

            val outputFile = new FileOutputStream(dumpOutputFilePath)

            for (i <- 0 until (size)) {
                outputFile.write(intToByteArray(this(i)))
            }

            def intToByteArray(value: Int): Array[Byte] = {
                Array[Byte](value.toByte, (value >> 8).toByte, (value >> 16).toByte, (value >> 24).toByte)
            }
        }
    }
}
