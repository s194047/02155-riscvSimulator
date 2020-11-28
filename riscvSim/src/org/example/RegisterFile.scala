package org.example

import java.io._

class RegisterFile(private val size: Int, private val offset: Int = 1, private val verbose: Boolean = true, private val dumpOutputFilePathOpt: Option[String]) {
    private val data = new Array[Int](size - offset)

    def apply(register: Int): Int = if (register > 0) data(register - offset) else 0
    def update(register: Int, value: Int): Unit = {
        if (register - offset >= 0) {
            data(register - offset) = value
        }
    }

    def printRegisters(inline: Boolean = false): Unit = {
        if (verbose) {
            println("Register contents:")
            if (inline) {
                for (i <- 0 until size - offset) {
                    print(f"x${i + offset}%02d: ${data(i)}" + (if (i == size - offset - 1) "" else " "))
                }
                println
            } else {
                for (i <- 0 until size - offset) {
                    println(f"x${i + offset}%02d: ${data(i)}")
                }
            }
        }
    }

    def writeRegisterDump: Unit = {
        for (dumpOutputFilePath <- dumpOutputFilePathOpt) {
            val outputFile = new FileOutputStream(dumpOutputFilePath)

            for (i <- 0 until (size)) {
                outputFile.write(intToByteArray(this(i)))
            }

            def intToByteArray(value: Int): Array[Byte] = {
                Array[Byte](value.toByte, (value << 8).toByte, (value << 16).toByte, (value << 24).toByte)
            }
        }
    }
}
