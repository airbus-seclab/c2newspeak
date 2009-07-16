Newspeak output
---------------
061.c
void main(void) {
  (061.c:38#1235)^int32 z;
  (061.c:39#1240)^0- =(int32) coerce[-2147483648,2147483647] (coerce[-2147483648,2147483647] (y_int32 + [x_ptr]8_int8) + [t_ptr]32_int32);
  (061.c:40#1260)^0- =(int32) coerce[-2147483648,2147483647] (a_int32 + b_int32);
}

int8[6] !061.c.const_str_Hello;
int32 a;
int32 b;
ptr t;
ptr x;
int32 y;

