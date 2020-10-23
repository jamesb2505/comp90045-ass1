# Roo Compiler - COMP90045 Project

This project contatins a compiler for the Roo language (see [asg1.pdf](https://github.com/jimbxb/comp90045-project/blob/master/spec/asg1.pdf) for more details), 
with a target assembly-like language Oz (see [asg3.pdf](https://github.com/jimbxb/comp90045-project/blob/master/spec/asg3.pdf) for more details).

The compiler is written in Haskell, and makes extensive use of the Haskell tools Happy and Alex, similar to Bison and Flex for C/C++.

## Build
```sh
$ make
$ cd oz
$ make
$ cd ..
```

## Run
```sh
$ ./Roo prog.roo > prog.oz
$ ./oz/oz prog.oz
```

## Other Features
* Pretty Print a Roo program

  ```sh
  $ ./Roo -p prog.roo
  ```
  
* AST for a Roo program

  ```sh
  $ ./Roo -a prog.roo
  ```
  
* Roo -> C transcompilation

  ```sh
  $ ./Roo -c prog.roo > proc.c
  $ gcc -O3 prog.c -o prog
  $ ./prog
  ```
  
* Roo -> Python transcompilation

  ```sh
  $ ./Roo -py prog.roo > prog.py
  $ python3 prog.py
  ```

## Samples

* "Hello, World!" in Roo
```
procedure main ()
{
    writeln "Hello, World!";
}
```

* Translated into OZ
```
call proc_main
halt
proc_main:
    string_const r0, "Hello, World!"
    call_builtin print_string
    call_builtin print_newline
    return
```
  
## Grades
* Part 1: Parser and Pretty Printer
  
  11.5/12 (0.5 lost due to code style)
  
* Part 2: Individual Peer Reviews

  Withheld
  
* Part 3: Code Generation

  TBC
