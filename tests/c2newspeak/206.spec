Warning: 206.c:30#2: expression of type signed integer used as an array index ignored
Newspeak output
---------------
206.c
void main(void) {
  (206.c:27#7)^int8[10] t;
  (206.c:28#6)^int32 x;
  (206.c:30#2)^1- + (belongs[0,9] 0-_int32 * 32) =(int8) 1;
  (206.c:30#2)^0- =(int32) coerce[-2147483648,2147483647] (0-_int32 + 1);
}


