Warning: conditional expression are ugly: use if else instead in 297.c line 30
Newspeak output
---------------
297.c
main() {
  (297.c:30#1086)^ptr tmp0;
  (297.c:30#1086)^choose {
    | ! (x_int32 ==_int32 0) -->
      (297.c:30#1086)^0- =(ptr) &_32(x);
    | (x_int32 ==_int32 0) -->
      (297.c:30#1086)^0- =(ptr) &_32(x);
  }
  (297.c:30#1086)^ptr =(ptr) 0-_ptr;
}

ptr ptr = 0;
int32 x = 0;

