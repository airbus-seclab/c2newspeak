Warning: halting condition should be explicit in 296.c line 28
Newspeak output
---------------
296.c
main() {
  (296.c:27#1072)^int32 i;
  (296.c:28#1082)^0- =(int32) 0;
  (296.c:28#1077)^while (1) {
    (296.c:28#1091)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  }
}


