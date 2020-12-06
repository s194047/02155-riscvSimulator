package org.example

object Instruction {
    def readInstructionType(memory: Memory, address: Int): Instruction = {
        val instructionInt = memory.readWord(address)
        val opcode = instructionInt & 0x7f

        opcode match {
            case 0x33 => {
                val funct7 = (instructionInt & 0xfe000000) >> 25
                val rs2    = (instructionInt &  0x1f00000) >> 20
                val rs1    = (instructionInt &    0xf8000) >> 15
                val funct3 = (instructionInt &     0x7000) >> 12
                val rd     = (instructionInt &      0xf80) >> 7

                new RTypeInstruction(funct7, rs2, rs1, funct3, rd)
            }
            case 0x03 | 0x13 | 0x67  => {
                val immediate = (instructionInt & 0xfff00000) >> 20
                val rs1       = (instructionInt &    0xf8000) >> 15
                val funct3    = (instructionInt &     0x7000) >> 12
                val rd        = (instructionInt &      0xf80) >> 7

                new ITypeInstruction(immediate, rs1, funct3, rd, opcode)
            }
            case 0x23 => {
                val immediate = ((instructionInt & 0xfe000000) >> 20) + ((instructionInt & 0xf80) >> 7)
                val rs2       = (instructionInt &  0x1f00000) >> 20
                val rs1       = (instructionInt &    0xf8000) >> 15
                val funct3    = (instructionInt &     0x7000) >> 12

                new STypeInstruction(immediate, rs2, rs1, funct3)
            }
            case 0x63 => {
                val immediate = ((instructionInt & 0x80000000) >> 19) + ((instructionInt & 0x7e000000) >> 20) +
                                ((instructionInt & 0xf00) >> 7) + ((instructionInt & 0x80) << 4)
                val rs2       = (instructionInt &  0x1f00000) >> 20
                val rs1       = (instructionInt &    0xf8000) >> 15
                val funct3    = (instructionInt &     0x7000) >> 12

                new SBTypeInstruction(immediate, rs2, rs1, funct3)
            }
            case 0x17 | 0x37 => {
                val immediate = instructionInt & 0xfffff000
                val rd        = (instructionInt &      0xf80) >> 7

                new UTypeInstruction(immediate, rd, opcode)
            }
            case 0x6f => {
                val immediate = ((instructionInt & 0x80000000) >> 11) + ((instructionInt & 0x7fe00000) >> 20) +
                                ((instructionInt & 0x100000) >> 9) + (instructionInt & 0xff000)
                val rd        = (instructionInt &      0xf80) >> 7

                new UJTypeInstruction(immediate, rd)
            }
            case 0x73 => new EnvInstruction
            case _    => new InvalidInstruction
        }
    }
}

sealed trait Instruction {
    def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit
}

class RTypeInstruction(private val funct7: Int,
                         private val rs2: Int,
                         private val rs1: Int,
                         private val funct3: Int,
                         private val rd: Int) extends Instruction {

    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        registers(rd) = (funct7, funct3) match {
            case (0x00, 0x0) => registers(rs1) +  registers(rs2) // add
            case (0x20, 0x0) => registers(rs1) -  registers(rs2) // sub
            case (0x00, 0x1) => registers(rs1) << registers(rs2) // sll
            case (0x00, 0x2) => if (registers(rs1) < registers(rs2)) 1 else 0 // slt
            case (0x00, 0x3) => if (registers(rs1) < (registers(rs2) & 0xfff)) 1 else 0 // sltu
            case (0x00, 0x4) => registers(rs1) ^  registers(rs2) // xor
            case (0x00, 0x5) => registers(rs1) >>> registers(rs2) // srl
            case (0x20, 0x5) => registers(rs1) >> registers(rs2) // sra
            case (0x00, 0x6) => registers(rs1) |  registers(rs2) // or
            case (0x00, 0x7) => registers(rs1) &  registers(rs2) // and
            case _           => throw new InvalidInstructionExecption("Unrecognised R-type instruction")
        }

        programCounter += 4
    }

    override def toString(): String = s"R-Type(funct7: $funct7, rs2: $rs2, rs1: $rs1, funct3: $funct3, rd: $rd)"
}

class ITypeInstruction(private val immediate: Int,
                         private val rs1: Int,
                         private val funct3: Int,
                         private val rd: Int,
                         private val opcode: Int) extends Instruction {

    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        registers(rd) = (funct3, opcode) match {
            case (0x0, 0x03) => memory.readByte(immediate + registers(rs1)) // lb
            case (0x1, 0x03) => memory.readHalfword(immediate + registers(rs1)) // lh
            case (0x2, 0x03) => memory.readWord(immediate + registers(rs1)) // lw
            case (0x4, 0x03) => memory.readByteUnsigned(immediate + registers(rs1)) // lbu
            case (0x5, 0x03) => memory.readHalfwordUnsigned(immediate + registers(rs1)) // lhu

            case (0x0, 0x13) => registers(rs1) + immediate // addi
            case (0x2, 0x13) => if (registers(rs1) < immediate) 1 else 0 // slti
            case (0x3, 0x13) => if (registers(rs1) < (immediate & 0xfff)) 1 else 0 // sltiu
            case (0x4, 0x13) => registers(rs1) ^ immediate // xori
            case (0x6, 0x13) => registers(rs1) | immediate // ori
            case (0x7, 0x13) => registers(rs1) & immediate // andi
            case (0x1, 0x13) => {
                val shamt = immediate & 0x1f

                registers(rs1) << shamt // slli
            }
            case (0x5, 0x13) => {
                val shamt = immediate & 0x1f

                if ((immediate & 0xfe0) == 0x0) {
                    registers(rs1) >>> immediate // srli
                } else {
                    registers(rs1) >> immediate // srai
                }
            }
            case (0x0, 0x67) => { // jalr
                val returnAddress = programCounter() + 4 // calculate return address

                programCounter() = immediate + registers(rs1) // Update the pc

                returnAddress
            }
            case _           => throw new InvalidInstructionExecption("Unrecognised I-type instruction")
        }

        if ((funct3, opcode) != (0x0, 0x67)) {
            programCounter += 4 // Update pc on all other instructions than jalr
        }
    }

    override def toString(): String = s"I-Type(immediate: $immediate, rs1: $rs1, funct3: $funct3, rd: $rd)"
}

class STypeInstruction(private val immediate: Int,
                         private val rs2: Int,
                         private val rs1: Int,
                         private val funct3: Int) extends Instruction {
    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        funct3 match {
            case 0x0 => memory.writeByte(immediate + registers(rs1), registers(rs2)) // sb
            case 0x1 => memory.writeHalfword(immediate + registers(rs1), registers(rs2)) // sh
            case 0x2 => memory.writeWord(immediate + registers(rs1), registers(rs2)) // sw
            case _   => throw new InvalidInstructionExecption("Unrecognised S-type instruction")
        }

        programCounter += 4
    }

    override def toString(): String = s"S-Type(immediate: $immediate, rs1: $rs1, rs2: $rs2, funct3: $funct3)"
}

class SBTypeInstruction(private val immediate: Int,
                          private val rs2: Int,
                          private val rs1: Int,
                          private val funct3: Int) extends Instruction {

    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        programCounter += (funct3 match {
            case 0x0 => if (registers(rs1) == registers(rs2)) immediate else 4 // beq
            case 0x1 => if (registers(rs1) != registers(rs2)) immediate else 4 // bne
            case 0x4 => if (registers(rs1) <  registers(rs2)) immediate else 4 // blt
            case 0x5 => if (registers(rs1) >= registers(rs2)) immediate else 4 // bge
            case 0x6 => if ((registers(rs1).toLong & 0xffffffff) <  (registers(rs1).toLong & 0xffffffff)) {
                immediate // bltu
            } else {
                4
            }
            case 0x7 => if ((registers(rs1).toLong & 0xffffffff) >= (registers(rs1).toLong & 0xffffffff)) {
                immediate // bgeu
            } else {
                4
            }
            case _   => throw new InvalidInstructionExecption("Unrecognised SB-type instruction")
        })
    }

    override def toString(): String = s"SB-Type(immediate: $immediate, rs1: $rs1, rs2: $rs2, funct3: $funct3)"
}

class UTypeInstruction(private val immediate: Int,
                         private val rd: Int,
                         private val opcode: Int) extends Instruction {

    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        registers(rd) = {
            opcode match {
                case 0x37 => immediate // lui
                case 0x17 => immediate + programCounter() // auipc
            }
        }

        programCounter += 4
    }

    override def toString(): String = s"U-Type(immediate: $immediate, rd: $rd, opcode: $opcode)"
}

class UJTypeInstruction(private val immediate: Int,
                          private val rd: Int) extends Instruction {

    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        // jal
        registers(rd) = programCounter() + 4
        programCounter += immediate
    }

    override def toString(): String = s"UJ-Type(immediate: $immediate, rd: $rd)"
}

class EnvInstruction extends Instruction {

    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        // ecall (based on the environmental calls in the Venus simulator)
        registers(10) match {
            case 1       => print(registers(11)) // Print contents of a1 as an int
            case 4       => print(memory.readString(registers(11))) // Print string starting at address in a1
            case 9       => ??? // FIXME: Implement this call? (Allocate a1 bytes on the heap, return pointer to start in a0)
            case 10 | 17 => {
                // Exit the program (status 0 if a0 == 10 and status a1 if a0 == 17)
                val status = if (registers(10) == 10) 0 else registers(11)
                registers.printRegisters()
                registers.writeRegisterDump
                System.exit(status)
            }
            case 11       => print(registers(11).toChar) // Print contents of a1 as an ASCII character
            case _        => throw new InvalidInstructionExecption("Unrecognised ID for ecall")
        }

        programCounter += 4
    }

    override def toString(): String = "Environment call"
}

class InvalidInstruction extends Instruction {

    override def execute(registers: RegisterFile, memory: Memory, programCounter: ProgramCounter): Unit = {
        throw new InvalidInstructionExecption("Unrecognised instruction type")
    }

    override def toString(): String = "Invalid instruction"
}

class InvalidInstructionExecption(message: String) extends Exception(message) {
    def this(message: String, cause: Throwable) {
        this(message)
        initCause(cause)
    }

    def this(cause: Throwable) {
        this(Option(cause).map(_.toString).orNull, cause)
    }

    def this() {
        this(null: String)
    }
}
