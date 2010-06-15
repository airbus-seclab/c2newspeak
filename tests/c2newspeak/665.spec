Warning: 665.c:26#0: 'long long unsigned int' is not normalized : use 'unsigned long long int' instead
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Newspeak output
---------------
665.c
void main(void) {
  (665.c:26#25)^uint64 i;
  (665.c:27#2)^i =(uint64) 0;
}


