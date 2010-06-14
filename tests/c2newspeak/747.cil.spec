Newspeak output
---------------
747.c
void main(void) {
  (747.c:27#1052)^int32 rnd;
  (747.c:28#1063)^ptr x;
  (747.c:29#1074)^ptr ptr;
  (747.c:31#1082)^choose {
   -->
    (747.c:31#1082)^guard(! (rnd_int32 ==_int32 0));
    (747.c:31#1082)^x =(ptr) ptr_ptr;
   -->
    (747.c:31#1082)^guard((rnd_int32 ==_int32 0));
    (747.c:31#1082)^x =(ptr) nil;
  }
}


