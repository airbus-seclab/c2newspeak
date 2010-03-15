Warning: 383.c:33#2: expression of type signed integer used as an array index ignored
Warning: 383.c:33#2: expression of type signed integer used as an array index ignored
Newspeak output
---------------
383.c
void main(void) {
  (383.c:30#6)^int32[2][2] y;
  (383.c:31#7)^ptr ptr;
  (383.c:33#2)^0- =(ptr) (focus64 &(1- + (belongs[0,1] i_int32 * 64)) + (j_int32 * 32));
}

int32 i;
int32 j;

