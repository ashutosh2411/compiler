# compiler
Compilers Lab @ IIT Palakkad.

Compiler Project as a part of Language Translators Laboratory (CS3310)

Group_number 11

## Contributors: 
  - Ashutosh Upadhye
  - Vishal Kumar Chaudhary

## Execution: 
Navigate to compiler/parser, then execute the following commands. 
```
sml
```
Then in the interpreter, enter the following commands. 
```
CM.make("sources.cm");
Control.Print.printDepth:=20;
Control.Print.printLength:=20;
Control.Print.stringDepth:=20;
Parser.parse "test.av";
```

## Repositories: 
  - lexer : Phase 1 of the project. Tokenizes the input into a stream of tokens. 
  - parser : Phase 2 of the project. Parses the stream of tokens and generates a syntax tree. 
  - codegen : Phase 3 of the project. Aims at generating Javascript from the AST generated in previous stages. 

## About the project: 
  - The aim is to convert a code written in AV, a C like language, to javascript. 

## The AV language: 
AV is an imperative language. It has no function declerations. The following are specifications of AV. 
### Datatypes supported: 
  - The current version of AV supports only integers. 
### Conditional and if statements: 
  - AV supports if, if-else statements and for loops. The syntax is similar to C. 
### Functions: 
  - Current version of AV doesn't support function calls. 

## Licence: 
  - This is an intellectual property of Vishal Kumar Chaudhary and Ashutosh Upadhye. Code can be used for educational purposes with proper acknowledgements. 

