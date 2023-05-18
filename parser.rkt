#lang brag
mod : line-separator [SEXP line-separator] program
program : (statement line-separator)* [statement]
statement : set | mark | exec | send | yield | reset | show
set : "set" ID SEXP
mark : "mark" ID INT
exec : "exec" SEXP
send : "send" pos pos ID*
yield : "yield" pos pos
reset : "reset"
show : "show"
line-separator : NL+
pos : INT | ID

