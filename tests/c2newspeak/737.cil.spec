Newspeak output
---------------
737.c
void main(void) {
  (737.c:28#1057)^int32 x;
  (737.c:29#1066)^ptr ptr;
  (737.c:30#1074)^choose {
   -->
    (737.c:30#1074)^guard(! ([ptr_ptr]32_int32 ==_int32 0));
    (737.c:30#1074)^x =(int32) 1;
   -->
    (737.c:30#1074)^guard(([ptr_ptr]32_int32 ==_int32 0));
    (737.c:30#1074)^x =(int32) 0;
  }
}


