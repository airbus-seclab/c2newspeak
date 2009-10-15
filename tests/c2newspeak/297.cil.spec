Newspeak output
---------------
297.c
void main(void) {
  (297.c:30#1086)^choose {
   -->
    (297.c:30#1086)^guard(! (x_int32 ==_int32 0));
    (297.c:30#1086)^ptr =(ptr) focus32 &(x);
   -->
    (297.c:30#1086)^guard((x_int32 ==_int32 0));
    (297.c:30#1086)^ptr =(ptr) focus32 &(x);
  }
}

ptr ptr;
int32 x;

