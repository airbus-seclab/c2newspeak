Newspeak output
---------------
void main(void) {
  (575.c:27#6)^int32 x;
  (575.c:28#2)^choose {
   -->
    (575.c:28#2)^guard(! (coerce[-2147483648,2147483647] (x_int32 + 1) ==_int32 0));
   -->
    (575.c:28#2)^guard((coerce[-2147483648,2147483647] (x_int32 + 1) ==_int32 0));
  }
}


