Newspeak output
---------------
297.c
void main(void) {
  (297.c:30#1086)^choose {
    | ! (x_int32 ==_int32 0) -->
      (297.c:30#1086)^ptr =(ptr) &_32(x);
    | (x_int32 ==_int32 0) -->
      (297.c:30#1086)^ptr =(ptr) &_32(x);
  }
}

ptr ptr = 0;
int32 x = 0;

