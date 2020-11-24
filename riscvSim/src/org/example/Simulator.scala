package org.example

import java.io._
import scala.language.postfixOps

object Simulator {
    def main(args: Array[String]): Unit = {

        // Parse arguments
        val filePath = if (args.length > 0) args(0) else ""
        val memorySize = if (args.length > 1) {
            args(1).toIntOption.getOrElse(4194304) // 4 MiB
        } else 4194304
        val printRegistersOnExit = if (args.length > 2) {
            args(2) == "true"
        } else false

        // Read from file into Byte Array.
        val program = fileToByteArray(filePath)

        // Initialise memory and load program into memory
        val memory = new Memory(memorySize)
        memory.loadMemory(program, 0)

        // Initialise registers
        val registers = new RegisterFile(size = 32, offset = 1, verbose = printRegistersOnExit)

        // Main processor loop
        val programCounter = new ProgramCounter(0)
        while (programCounter() <= memory.size - 4) {
            // Fetch instruction from memory
            val instruction = Instruction.readInstructionType(memory, programCounter())

            // debug
            println(f"${programCounter()}%04d" + ": " + instruction.toString)
            //println(f"${memory.readWord(programCounter())}%8h")
            println(registers.printRegisters(inline = true))
            println(memory.readWord(programCounter()).toHexString)
            println()

            instruction.execute(registers, memory, programCounter)
        }
    }

    def fileToByteArray(filePath: String): Array[Byte] = {
        if (filePath == "") {
            new Array[Byte](0)
        }
        else {
            val bis = new BufferedInputStream(new FileInputStream(filePath))
            Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray
        }
    }
}
