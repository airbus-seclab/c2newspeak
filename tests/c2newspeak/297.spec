Newspeak output
---------------
297.c
void main(void) {
  (297.c:30#2)^choose {
   -->
    (297.c:30#2)^guard(! (x_int32 ==_int32 0));
    (297.c:30#2)^ptr =(ptr) &_32(x);
   -->
    (297.c:30#2)^guard((x_int32 ==_int32 0));
    (297.c:30#2)^ptr =(ptr) &_32(x);
  }
}

ptr ptr = 0;
int32 x = 0;

