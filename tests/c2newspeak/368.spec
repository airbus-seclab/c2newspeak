Newspeak output
---------------
368.c
main() {
  (368.c:31#1112)^int8[1] tmp0;
  (368.c:31#1112)^0- =(uint1) x_uint1;
  (368.c:31#1112)^{
    ptr printf.arg1;
    (368.c:31#1112)^0- =(ptr) &_32(!368.c.const_str_%d\n);
    (368.c:31#1112)^{
      ptr printf.arg2;
      (368.c:31#1112)^0- =(ptr) &_8(2-);
      (368.c:31#1112)^printf();
    }
  }
}

int8[4] !368.c.const_str_%d\n = {0: int8 37;8: int8 100;16: int8 10;24: int8 0};
uint1 x = 0;
