Warning: 296.c:28#0: halting condition should be explicit
Newspeak output
---------------
void main(void) {
  (296.c:27#6)^int32 i;
  (296.c:28#7)^i =(int32) 0;
  (296.c:28#2)^while (1) {
    (296.c:28#16)^i =(int32) coerce[-2147483648,2147483647] (i_int32 + 1);
  }
}


