#lang brag
program : [newline] (statement newline)* [statement]
statement : set | mark | exec | send | yield | reset
set : "set" ID SEXP
mark : "mark" ID INT
exec : "exec" SEXP
send : "send" pos pos ID*
yield : "yield" pos pos
reset : "reset"
newline : NL+
pos : INT | ID

