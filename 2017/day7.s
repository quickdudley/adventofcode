# RISC-V assembler
# Linux syscall numbers from https://rv8.io/syscalls.html
# Syscall arguments from https://syscalls.kernelgrok.com/
.section .text
.globl _start
_start:
  # Open input data file
  lui a0, %hi(datafile)
  addi a0, a0, %lo(datafile)
  li a1, 0 # O_RDONLY
  li a2, 0
  li a7, 1024 # open
  ecall
  addi s2, a0, 0 # save file descriptor to s2

  # Close input data file
  addi a0, s2, 0
  li a7, 57
  ecall

  # exit
  li a0, 0
  li a1, 0
  li a2, 0
  li a3, 0
  li a7, 93
  ecall

.section .rodata
datafile:
  .string "day7.txt"
