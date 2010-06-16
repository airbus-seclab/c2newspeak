Warning: 511.c:30#0: comma terminated initializer
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Newspeak output
---------------
511.c
void main(void) {
}

int32[3] x;
(511.c:26#4)^x =(int32) 1;
(511.c:26#4)^x + 32 =(int32) 2;
(511.c:26#4)^x + 64 =(int32) 3;

