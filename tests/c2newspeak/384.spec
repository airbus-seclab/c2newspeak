Warning: 384.c:38#2: expression of type signed integer used as an array index accepted
Newspeak output
---------------
void (384.c:34#5)^main(void) {
  (384.c:35#4)^{ int32 0; int32 32; }64[2] y;
  (384.c:36#7)^ptr ptr;
  (384.c:38#2)^ptr =(ptr) focus64 &(y + (belongs[0,1] i_int32 * 64) + 32);
}

int32 i;

