# Program for day 5

The program is written in Applesoft BASIC inside the linapple emulator. It is
in the Applesoft BASIC compact format.

## Running

You can copy the program into a disk image using the tools at
https://github.com/deater/dos33fsprogs.

You can then run the program using https://github.com/dabonetn/linapple-pie
(or a real Apple ][ if you have one and are able to transfer data to it)

The program looks for a text file named "DAY5TEXT" for input. There are some
differences between this file and the raw puzzle input:

* The high bit for each character must be set (`ascii | 0x80`)
* The line separator is byte 0x8d (`'\r' | 0x80`)