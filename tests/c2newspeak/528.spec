Newspeak output
---------------
528.c
main() {
  (528.c:27#8)^ptr y;
  (528.c:28#8)^ptr dst;
  (528.c:29#8)^ptr src;
  (528.c:30#15)^uint32 n;
  (528.c:32#2)^ptr value_of___builtin_strncpy;
  (528.c:32#2)^{
    ptr __builtin_strncpy.arg1;
    (528.c:32#2)^0- =(ptr) 4-_ptr;
    (528.c:32#2)^{
      ptr __builtin_strncpy.arg2;
      (528.c:32#2)^0- =(ptr) 4-_ptr;
      (528.c:32#2)^{
        uint32 __builtin_strncpy.arg3;
        (528.c:32#2)^0- =(uint32) 4-_uint32;
        (528.c:32#2)^__builtin_strncpy();
      }
    }
  }
  (528.c:32#2)^4- =(ptr) 0-_ptr;
}


