Newspeak output
---------------
574.c
void main(void) {
  (574.c:27#1072)^int32 x;
  (574.c:28#1077)^choose {
   -->
    (574.c:28#1077)^guard(! ((x_int32 & 1) ==_int32 0));
   -->
    (574.c:28#1077)^guard(((x_int32 & 1) ==_int32 0));
  }
}


