Newspeak output
---------------
110.c
void main(void) {
  (110.c:29#11)^int32 !tmp0;
  (110.c:29#11)^f();
  (110.c:29#2)^choose {
   -->
    (110.c:29#2)^guard(! (0-_int32 ==_int32 0));
   -->
    (110.c:29#2)^guard((0-_int32 ==_int32 0));
  }
}


