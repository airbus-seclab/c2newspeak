Warning: 513.c:29#0: conditional expression
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

