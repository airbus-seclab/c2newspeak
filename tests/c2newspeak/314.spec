Warning: assignment within expression accepted in 314.c line 29
Newspeak output
---------------
314.c
main() {
  (314.c:27#6)^int32 x;
  (314.c:29#14)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  (314.c:29#2)^choose {
    | ! (0-_int32 ==_int32 0) -->
    | (0-_int32 ==_int32 0) -->
  }
}


