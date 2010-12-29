Warning: 777.c:33#2: expression of type signed integer used as an array index accepted
Newspeak output
---------------
void (777.c:29#5)^main(void) {
  (777.c:30#6)^int32 a;
  (777.c:31#7)^int8[20] tab;
  (777.c:33#2)^tab + (belongs[0,19] coerce[-2147483648,2147483647] (0 - a_int32) * 8) =(int8) 5;
}


