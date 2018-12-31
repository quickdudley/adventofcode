# RISC-V assembler
# Linux syscall numbers from https://git.qemu.org/?p=qemu.git;a=blob;f=linux-user/riscv/syscall_nr.h;h=7e30f1f1ef48ddbb1620d2772df7df3c9e0ae200;hb=refs/heads/master
# Syscall arguments from https://syscalls.kernelgrok.com/

# Register usage:
# s2: data fd
# s3: number of bytes read
# s4: number of bytes parsed
# s5: jump destination for parsing next character
# s6: character
# s7: pointer to tree node
.section .text
.globl _start
_start:
  # Open input data file
  li a0, -100 # AT_FDCWD
  lui a1, %hi(datafile)
  addi a1, a1, %lo(datafile)
  li a2, 0 # O_RDONLY
  li a3, 0
  li a7, 56 # open
  ecall
  addi s2, a0, 0 # save file descriptor to s2

readloop:
  # Read 1024 bytes from the file
  addi a0, s2, 0
  lui a1, %hi(readbuf)
  addi a1, a1, %lo(readbuf)
  li a2, 1024
  li a7, 63 # read
  ecall

  bge zero, a0, on_eof
  addi s3, a0, 0 # s3: number of bytes read
  addi s4, zero, 0 # s4: number of bytes parsed
  # TODO parse the buffer
  j readloop

on_eof:
  # Close input data file
  addi a0, s2, 0
  li a7, 57
  ecall

  # exit
  addi a0, s2, 0
  li a1, 0
  li a2, 0
  li a3, 0
  li a7, 93
  ecall

.section .rodata
datafile:
  .string "day7.txt"
dfmt:
  .string "%d\n"
.section .data
  .p2align 4, 0

memroot:
  .zero 1024
readbuf:
  .zero 1024
