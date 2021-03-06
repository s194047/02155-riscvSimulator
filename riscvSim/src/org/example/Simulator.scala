package org.example

import java.io._

import mainargs.{main, arg, ParserForMethods, Flag}

object Simulator {
    @main
    def run(
        @arg(name = "program-path", short = 'f', doc = "Path to RISC-V program to be executed")
        programPath: String,
        @arg(name = "memory-size", short = 'm', doc = "Size of memory (default is 4 MiB)")
        memorySize: Int = 4194304,
        @arg(name = "print-registers", short = 'p', doc = "Print registers on exit")
        printRegistersOnExit: Flag,
        @arg(name = "instruction-trace", short = 't', doc = "Print instruction and register trace during execution")
        trace: Flag,
        @arg(name = "dump-registers-file", short = 'd', doc = "Dump registers to file on exit")
        dumpRegistersFilePathOpt: Option[String] = None
    ): Unit = {
        // Read from file into Byte Array.
        val program = fileToByteArray(programPath)

        // Initialise memory and load program into memory
        val memory = new Memory(memorySize)
        memory.loadMemory(program, 0)

        // Initialise registers
        val registers = new RegisterFile(size = 32, offset = 1, verbose = printRegistersOnExit.value,
                dumpOutputFilePathOpt = dumpRegistersFilePathOpt)

        // Main processor loop
        val programCounter = new ProgramCounter(0)
        while (programCounter() <= memory.size - 4) {
            // Fetch and decode instruction from memory
            val instruction = Instruction.readInstructionType(memory, programCounter())

            // Print trace
            if (trace.value) {
                println(f"${programCounter()}%04d" + ":")
                println(instruction.toString)
                println(f"0x${memory.readWord(programCounter())}%08x")
                registers.printRegisters(inline = true)
                println
            }

            // Execute instruction
            instruction.execute(registers, memory, programCounter)
        }

        throw new RuntimeException("Program counter exceeded memory size.")
    }

    def main(args: Array[String]): Unit = ParserForMethods(this).runOrExit(args)

    def fileToByteArray(filePath: String): Array[Byte] = {
        if (filePath == "") {
            new Array[Byte](0)
        }
        else {
            val bis = new BufferedInputStream(new FileInputStream(filePath))
            LazyList.continually(bis.read).takeWhile(_ != -1).map(_.toByte).toArray
        }
    }
}
