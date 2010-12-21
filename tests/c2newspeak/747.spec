Newspeak output
---------------
void main(void) {
  (747.c:27#5)^int32 rnd;
  (747.c:28#8)^ptr x;
  (747.c:29#7)^ptr ptr;
  (747.c:31#1)^choose {
   -->
    (747.c:31#1)^guard(! (rnd_int32 ==_int32 0));
    (747.c:31#1)^x =(ptr) ptr_ptr;
   -->
    (747.c:31#1)^guard((rnd_int32 ==_int32 0));
    (747.c:31#1)^x =(ptr) nil;
  }
}


