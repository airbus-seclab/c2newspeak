Warning: 206.c:30#2: expression of type signed integer used as an array index accepted
Newspeak output
---------------
206.c
void main(void) {
  (206.c:27#7)^int8[10] t;
  (206.c:28#6)^int32 x;
  (206.c:30#2)^t + (belongs[0,9] x_int32 * 8) =(int8) 1;
  (206.c:30#2)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
}


