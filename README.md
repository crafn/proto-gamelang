proto-gamelang
==============

Prototyping some language ideas for games. See plans directory for more info
####Compiling the compiler

    premake4 gmake
    make
  
####Compiling gamelang to binary

    ./compiler source_file

Compiler will output `temp.c` and use gcc to compile it to a binary named `out`

###TODO
- compiler: proper error messaging system
- ast examining tool
