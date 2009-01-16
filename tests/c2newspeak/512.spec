Newspeak output
---------------
512.c
void main(void) {
  (512.c:29#2)^choose {
   -->
    (512.c:29#2)^guard(! (x_int32 ==_int32 0));
   -->
    (512.c:29#2)^guard((x_int32 ==_int32 0));
  }
}

int32 x = 0;

