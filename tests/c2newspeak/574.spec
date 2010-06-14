Newspeak output
---------------
574.c
void main(void) {
  (574.c:27#6)^int32 x;
  (574.c:28#2)^choose {
   -->
    (574.c:28#2)^guard(! ((x_int32 & 1) ==_int32 0));
   -->
    (574.c:28#2)^guard(((x_int32 & 1) ==_int32 0));
  }
}


