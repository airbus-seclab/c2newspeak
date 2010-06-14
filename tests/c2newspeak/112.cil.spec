Newspeak output
---------------
112.c
void main(void) {
  (112.c:27#1072)^int32 x;
  (112.c:28#1077)^choose {
   -->
    (112.c:28#1077)^guard((x_int32 ==_int32 0));
   -->
    (112.c:28#1077)^guard(! (x_int32 ==_int32 0));
  }
}


