Warning: 513.c:29#0: conditional expression
Newspeak output
---------------
void (513.c:28#5)^main(void) {
  (513.c:29#2)^choose {
   -->
    (513.c:29#2)^guard(! (x_int32 ==_int32 0));
   -->
    (513.c:29#2)^guard((x_int32 ==_int32 0));
  }
}

int32 x;

