#lang brag
program : [newline] (statement newline)* [statement]
statement : definition | operation
newline : NL+
definition : ID SEXP
operation : operator (operand)*
operator : "init" | "jump" | "send" | "mark" | "yield"
operand : ID
