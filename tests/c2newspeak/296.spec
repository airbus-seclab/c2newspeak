Warning: 296.c:28: halting condition should be explicit
Newspeak output
---------------
296.c
main() {
  (296.c:27#6)^int32 i;
  (296.c:28#7)^0- =(int32) 0;
  (296.c:28#2)^while (1) {
    (296.c:28#16)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
  }
}


