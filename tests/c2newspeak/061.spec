Newspeak output
---------------
061.c
void main(void) {
  (061.c:38#6)^int32 z;
  (061.c:39#2)^0- =(int32) coerce[-2147483648,2147483647] (coerce[-2147483648,2147483647] (y_int32 + [x_ptr]8_int8) + [t_ptr]32_int32);
  (061.c:40#2)^0- =(int32) coerce[-2147483648,2147483647] (a_int32 + b_int32);
}

int8[6] !061.c.const_str_Hello = {0: int8 72;8: int8 101;16: int8 108;24: int8 108;32: int8 111;40: int8 0};
int32 a = {0: int32 2};
int32 b = {0: int32 3};
ptr t = {0: ptr &_32(y)};
ptr x = {0: ptr &_48(!061.c.const_str_Hello)};
int32 y = {0: int32 1};

