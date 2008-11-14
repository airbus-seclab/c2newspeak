Newspeak output
---------------
527.c
main() {
  (527.c:27#8)^ptr y;
  (527.c:28#8)^ptr dst;
  (527.c:29#8)^ptr src;
  (527.c:30#15)^uint32 n;
  (527.c:32#2)^ptr value_of___builtin_strncat;
  (527.c:32#2)^{
    ptr __builtin_strncat.arg1;
    (527.c:32#2)^0- =(ptr) 4-_ptr;
    (527.c:32#2)^{
      ptr __builtin_strncat.arg2;
      (527.c:32#2)^0- =(ptr) 4-_ptr;
      (527.c:32#2)^{
        uint32 __builtin_strncat.arg3;
        (527.c:32#2)^0- =(uint32) 4-_uint32;
        (527.c:32#2)^__builtin_strncat();
      }
    }
  }
  (527.c:32#2)^4- =(ptr) 0-_ptr;
}


