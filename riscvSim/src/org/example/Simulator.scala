package org.example

import java.io._
import scala.language.postfixOps

object Simulator {
    def main(args: Array[String]): Unit = {

        // Parse arguments
        val filePath = if (args.length > 0) args(0) else ""
        val memorySize = if (args.length > 1) {
            args(1).toIntOption.getOrElse(4096)
        } else 4096
        val printRegistersOnExit = if (args.length > 2) {
            args(2) == "true"
        } else false

        // Read from file into Byte Array.
        val program = fileToByteArray(filePath)

        // Initialise memory and load program into memory
        val memory = new Memory(memorySize)
        memory.loadMemory(program, 0)

        // Initialise registers
        val registers = new RegisterFile(32, 1)

        // Main processor loop
        var programCounter = 0
        while (programCounter <= memory.size - 4) {
            // Fetch instruction from memory
            val instruction = Instruction.readInstructionType(memory, programCounter)
            instruction.execute(registers)

            // debug
            println(instruction.toString())

            println(f"${memory.readWord(programCounter)}%8h")

            programCounter += 4
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
