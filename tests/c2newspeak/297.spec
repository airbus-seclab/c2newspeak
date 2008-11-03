Newspeak output
---------------
297.c
main() {
  (297.c:30#2)^ptr tmp0;
  (297.c:30#2)^choose {
    | ! (x_int32 ==_int32 0) -->
      (297.c:30#2)^0- =(ptr) &_32(x);
    | (x_int32 ==_int32 0) -->
      (297.c:30#2)^0- =(ptr) &_32(x);
  }
  (297.c:30#2)^ptr =(ptr) 0-_ptr;
}

ptr ptr = 0;
int32 x = 0;

