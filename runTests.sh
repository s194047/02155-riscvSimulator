#!/bin/bash -ex

out/riscvSim/assembly/dest/out.jar -f tests/task1/shift.bin -d testout/shift.out
out/riscvSim/assembly/dest/out.jar -f tests/task1/addpos.bin -d testout/addpos.out
out/riscvSim/assembly/dest/out.jar -f tests/task1/addneg.bin -d testout/addneg.out
out/riscvSim/assembly/dest/out.jar -f tests/task1/addlarge.bin -d testout/addlarge.out
out/riscvSim/assembly/dest/out.jar -f tests/task3/loop.bin -d testout/loop.out
out/riscvSim/assembly/dest/out.jar -f tests/task2/branchcnt.bin -d testout/branchcnt.out
out/riscvSim/assembly/dest/out.jar -f tests/task2/branchmany.bin -d testout/branchmany.out
out/riscvSim/assembly/dest/out.jar -f tests/venus-loop.bin -d testout/venus-loop.out
