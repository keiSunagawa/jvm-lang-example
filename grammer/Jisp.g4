grammar Jisp;

@header {
    package me.kerfume.jisp.antlr;
}


WS: [ \n\t]+ -> skip;
NUMBER: ('0' .. '9') +;
STRING: '"' .*? '"';
SYMBOl: ([a-zA-Z]+ ':')? [a-zA-Z]+;

error
 : UNEXPECTED_CHAR
   {
     throw new RuntimeException("UNEXPECTED_CHAR=" + $UNEXPECTED_CHAR.text);
   }
 ;

value: (NUMBER | SYMBOl | STRING);

list: '(' (value | list)* ')';

stmts: (list | error)* EOF;

UNEXPECTED_CHAR
 :  .
;
