Newspeak output
---------------
386.c
void main(void) {
  (386.c:27#8)^ptr t;
  (386.c:27#8)^0- =(ptr) &_96(!386.c.const_str_Hello world);
  (386.c:28#8)^{
    ptr u;
    (386.c:28#8)^0- =(ptr) &_96(!386.c.const_str_Hello world);
    (386.c:29#2)^[1-_ptr]8 =(int8) [0-_ptr]8_int8;
  }
}

int8[12] !386.c.const_str_Hello world = {0: int8 72;8: int8 101;16: int8 108;24: int8 108;32: int8 111;40: int8 32;48: int8 119;56: int8 111;64: int8 114;72: int8 108;80: int8 100;88: int8 0};

