Warning: avoid assignments within expressions in 314.c line 29
Newspeak output
---------------
314.c
main() {
  (314.c:27#1076)^int32 x;
  (314.c:29#1094)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  (314.c:29#1082)^choose {
    | ! (0-_int32 ==_int32 0) -->
    | (0-_int32 ==_int32 0) -->
  }
}


