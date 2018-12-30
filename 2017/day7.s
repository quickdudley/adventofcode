.section .text
.globl _start
_start:


  # exit
  li a0, 0
  li a1, 0
  li a2, 0
  li a3, 0
  li a7, 93
  ecall
.section .rodata
