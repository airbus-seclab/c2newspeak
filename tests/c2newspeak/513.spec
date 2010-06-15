Warning: 513.c:29#0: conditional expression
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Warning: __gnuc_builtin_symbols:1#0: 'long' is not normalized: use 'long int' instead
Newspeak output
---------------
513.c
void main(void) {
  (513.c:29#2)^choose {
   -->
    (513.c:29#2)^guard(! (x_int32 ==_int32 0));
   -->
    (513.c:29#2)^guard((x_int32 ==_int32 0));
  }
}

int32 x;

