Warning: 801.c:9#10: missing initializers for structure accepted
Newspeak output
---------------
801.c
{ ptr 0; ptr 32; ptr 64; { int32 0; }32 96; }128 x;
(801.c:9#10)^x + 32 =(ptr) nil;
(801.c:9#10)^x + 64 =(ptr) nil;
(801.c:9#10)^x + 96 =(int32) 0;
(801.c:9#10)^x =(ptr) nil;

