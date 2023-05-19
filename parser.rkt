#lang brag
program : line-separator [SEXP line-separator] body
body : (statement line-separator)* [statement]
statement : set | mark | exec | send | yield | reset
set : "set" ID SEXP
mark : "mark" ID INT
exec : "exec" SEXP
send : "send" pos pos ID*
yield : "yield" pos pos
reset : "reset"
line-separator : NL+
pos : INT | ID

