Newspeak output
---------------
void main(void) {
  (737.c:28#6)^int32 x;
  (737.c:29#7)^ptr ptr;
  (737.c:30#2)^choose {
   -->
    (737.c:30#2)^guard(! ([ptr_ptr]32_int32 ==_int32 0));
    (737.c:30#2)^x =(int32) 1;
   -->
    (737.c:30#2)^guard(([ptr_ptr]32_int32 ==_int32 0));
    (737.c:30#2)^x =(int32) 0;
  }
}


