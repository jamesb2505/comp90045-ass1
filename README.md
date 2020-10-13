# Roo Compiler - COMP90045 Project

This project contatins a compiler for the Roo language (see [asg1.pdf](https://github.com/jimbxb/comp90045-project/blob/master/spec/asg1.pdf) for more details), 
with a target interpretted assembly-like language Oz (see [asg3.pdf](https://github.com/jimbxb/comp90045-project/blob/master/spec/asg3.pdf) for more details).

The compiler is written in Haskell, and makes extensive use of the Haskell tools Happy and Alex, similar to Bison and Flex for C++.

## Build
```
$ make
$ cd oz
$ make
$ cd ..
```

## Run
```
./Roo prog.roo > prog.oz
./oz/oz prog.oz
```

# Other Features
* Pretty Print a Roo program

  ```
  ./Roo -p prog.roo
  ```
  
* AST for a Roo program

  ```
  ./Roo -a prog.roo
  ```
  
# Grades
* Part 1: Parser and Pretty Printer
  
  11.5/12 (0.5 lost due to code style)
* Part 2: Individual Peer Reviews

* Part 3: Code Generation

  TBC
