Warning: 385.c:31#2: expression of type signed integer used as an array index ignored
Newspeak output
---------------
385.c
void main(void) {
  (385.c:27#6)^int32 i;
  (385.c:28#6)^int32[2] t;
  (385.c:29#7)^ptr ptr;
  (385.c:31#2)^ptr =(ptr) (focus64 &(t) + (i_int32 * 32));
}


