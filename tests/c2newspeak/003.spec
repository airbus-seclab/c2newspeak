Warning: 003.c:38#2: expression of type signed integer used as an array index accepted
Newspeak output
---------------
void (003.c:31#5)^main(void) {
  (003.c:35#4)^{ int32 0; int32 32; }64 x;
  (003.c:36#6)^int32[10] t;
  (003.c:37#6)^int32 i;
  (003.c:38#2)^x + 32 =(int32) t + (belongs[0,9] i_int32 * 32)_int32;
}


