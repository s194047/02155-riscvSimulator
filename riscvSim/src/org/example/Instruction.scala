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

                new RFormatInstruction(funct7, rs2, rs1, funct3, rd)
            }
            case 0x03 | 0x13 | 0x67  => {
                val immediate = (instructionInt & 0xfff00000) >> 20
                val rs1       = (instructionInt &    0xf8000) >> 15
                val funct3    = (instructionInt &     0x7000) >> 12
                val rd        = (instructionInt &      0xf80) >> 7

                new IFormatInstruction(immediate, rs1, funct3, rd)
            }
            case 0x23 => {
                val immediate = ((instructionInt & 0xfe000000) >> 20) + ((instructionInt & 0xf80) >> 7)
                val rs2       = (instructionInt &  0x1f00000) >> 20
                val rs1       = (instructionInt &    0xf8000) >> 15
                val funct3    = (instructionInt &     0x7000) >> 12

                new SFormatInstruction(immediate, rs2, rs1)
            }
            case 0x63 => {
                val immediate = ((instructionInt & 0x80000000) >> 19) + ((instructionInt & 0x80) << 4) +
                                ((instructionInt & 0x7e000000) >> 20) + ((instructionInt & 0xf00) >> 7)
                val rs2       = (instructionInt &  0x1f00000) >> 20
                val rs1       = (instructionInt &    0xf8000) >> 15
                val funct3    = (instructionInt &     0x7000) >> 12

                new SBFormatInstruction(immediate, rs2, rs1, funct3)
            }
            case 0x37 => {
                val immediate = instructionInt & 0xfffff000
                val rd        = (instructionInt &      0xf80) >> 7

                new UFormatInstruction(immediate, rd)
            }
            case 0x6f => {
                val immediate = ((instructionInt & 0x80000000) >> 11) + (instructionInt & 0xff000) +
                                ((instructionInt & 0x100000) >> 9) + ((instructionInt & 0x7f800000) >> 20)
                val rd        = (instructionInt &      0xf80) >> 7

                new UJFormatInstruction(immediate, rd)
            }
            case 0x73 => new EnvInstruction
            case _    => new InvalidInstruction
        }
    }
}

sealed trait Instruction {
    def execute(registers: RegisterFile)
}

class RFormatInstruction(private val funct7: Int,
                         private val rs2: Int,
                         private val rs1: Int,
                         private val funct3: Int,
                         private val rd: Int) extends Instruction {

    override def execute(registers: RegisterFile): Unit = {
        registers(rd) = (funct7,funct3) match {
            case (0x00,0x0) => registers(rs1) +  registers(rs2) // add
            case (0x20,0x0) => registers(rs1) -  registers(rs2) // sub
            case (0x00,0x1) => registers(rs1) << registers(rs2) // sll
            case (0x00,0x2) => if (registers(rs1) < registers(rs2)) 1 else 0 // slt
            case (0x00,0x3) => if (registers(rs1) < (registers(rs2) & 0xfff)) 1 else 0 // sltu
            case (0x00,0x4) => registers(rs1) ^  registers(rs2) // xor
            case (0x00,0x5) => registers(rs1) >>> registers(rs2) // srl
            case (0x20,0x5) => registers(rs1) >> registers(rs2) // sra
            case (0x00,0x6) => registers(rs1) |  registers(rs2) // or
            case (0x00,0x7) => registers(rs1) &  registers(rs2) // and
            case _          => registers(rd)
        }
    }

    override def toString(): String = s"R-format(funct7: $funct7, rs2: $rs2, rs1: $rs1, funct3: $funct3, rd: $rd)"
}

class IFormatInstruction(private val immediate: Int,
                         private val rs1: Int,
                         private val funct3: Int,
                         private val rd: Int) extends Instruction {

    override def execute(registers: RegisterFile): Unit = {
        registers(rd) = funct3 match {
            case 0x0 => registers(rs1) + immediate // addi
            case 0x2 => if (registers(rs1) < immediate) 1 else 0 // slti
            case 0x3 => if (registers(rs1) < (immediate & 0xfff)) 1 else 0 // sltiu
            case 0x4 => registers(rs1) ^ immediate // xori
            case 0x6 => registers(rs1) | immediate // ori
            case 0x7 => registers(rs1) & immediate // andi
            case 0x1 => {
                val shamt = immediate & 0x1f

                registers(rs1) << shamt // slli
            }
            case 0x5 => {
                val shamt = immediate & 0x1f

                if ((immediate & 0xfe0) == 0x0) {
                    registers(rs1) >>> immediate // srli
                } else {
                    registers(rs1) >> immediate // srai
                }
            }
            case _   => registers(rd)
        }
    }

    override def toString(): String = s"I-format(immediate: $immediate, rs1: $rs1, funct3: $funct3, rd: $rd)"
}

class SFormatInstruction(private val immediate: Int,
                         private val rs2: Int,
                         private val rs1: Int) extends Instruction {

    override def execute(registers: RegisterFile): Unit = ???
}

class SBFormatInstruction(private val immediate: Int,
                          private val rs2: Int,
                          private val rs1: Int,
                          private val funct3: Int) extends Instruction {

    override def execute(registers: RegisterFile): Unit = ???
}

class UFormatInstruction(private val immediate: Int,
                         private val rd: Int) extends Instruction {

    override def execute(registers: RegisterFile): Unit = ???
}

class UJFormatInstruction(private val immediate: Int,
                          private val rd: Int) extends Instruction {

    override def execute(registers: RegisterFile): Unit = ???
}

class EnvInstruction extends Instruction {

    override def execute(registers: RegisterFile): Unit = {
        registers(10) match {
            case 10 | 17 => {
                val status = if (registers(10) == 10) 0 else registers(11)
                println("\nRegister contents:")
                registers.printRegisters()
                System.exit(status)
            }
            case _  => throw new RuntimeException("Illegal ID for ecall")
        }
    }
}

class InvalidInstruction extends Instruction {

  override def execute(registers: RegisterFile): Unit = throw new RuntimeException("Illegal instruction")
}
