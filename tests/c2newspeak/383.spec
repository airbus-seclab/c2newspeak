Warning: 383.c:33#2: expression of type signed integer used as an array index accepted
Warning: 383.c:33#2: expression of type signed integer used as an array index accepted
Newspeak output
---------------
void (383.c:29#5)^main(void) {
  (383.c:30#6)^int32[2][2] y;
  (383.c:31#7)^ptr ptr;
  (383.c:33#2)^ptr =(ptr) (focus64 &(y + (belongs[0,1] i_int32 * 64)) + (j_int32 * 32));
}

int32 i;
int32 j;

