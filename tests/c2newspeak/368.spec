Newspeak output
---------------
void (368.c:30#5)^main(void) {
  (368.c:31#2)^int8[1] tmp_firstpass!0;
  (368.c:31#2)^tmp_firstpass!0 =(uint1) x_uint1;
  (368.c:31#2)^printf(focus32 &(!cstr.%d\n): ptr, focus8 &(tmp_firstpass!0): ptr);
}

int8[4] !cstr.%d\n;
uint1 x;
(368.c:31#2)^!cstr.%d\n =(int8) 37;
(368.c:31#2)^!cstr.%d\n + 8 =(int8) 100;
(368.c:31#2)^!cstr.%d\n + 16 =(int8) 10;
(368.c:31#2)^!cstr.%d\n + 24 =(int8) 0;

