Newspeak output
---------------
206.c
void main(void) {
  (206.c:27#1073)^int8[10] t;
  (206.c:28#1086)^int32 x;
  int32 tmp;
  (206.c:30#1092)^tmp =(int32) x_int32;
  (206.c:30#1092)^x =(int32) coerce[-2147483648,2147483647] (x_int32 + 1);
  (206.c:30#1092)^t + (belongs[0,9] tmp_int32 * 8) =(int8) 1;
}


