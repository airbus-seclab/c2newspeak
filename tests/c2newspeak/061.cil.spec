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
(061.c:35#1206)^b =(int32) 3;
(061.c:34#1191)^a =(int32) 2;
(061.c)^!061.c.const_str_Hello =(int8) 72;
(061.c)^!061.c.const_str_Hello + 8 =(int8) 101;
(061.c)^!061.c.const_str_Hello + 16 =(int8) 108;
(061.c)^!061.c.const_str_Hello + 24 =(int8) 108;
(061.c)^!061.c.const_str_Hello + 32 =(int8) 111;
(061.c)^!061.c.const_str_Hello + 40 =(int8) 0;
(061.c:30#1141)^y =(int32) 1;
(061.c:29#1117)^x =(ptr) &_48(!061.c.const_str_Hello);
(061.c:31#1151)^t =(ptr) &_32(y);

