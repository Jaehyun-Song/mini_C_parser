# mini_C_parser

** This codes are made in the linux **

** This codes need flex & bison to execute **

1) You can make executable file through Makefile (just enter "make" at terminal)

2) ./ASTree < sample.c -> This sentence execute code.

3) This code generate 2 output files

  - table.txt
  
    This file includes symbol table. Symbol table's function starts to NULL due to case of global variable.
    A number which represents number of BRACE({, }) adjoins function name.
    Function names are listed in regular sequence.
    This table shows variable's count, type, name, status and length of array, and role.

  - tree.txt

    This file is output of input C code. It is code's abstract syntax tree.
