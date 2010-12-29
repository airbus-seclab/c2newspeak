Newspeak output
---------------
void (297.c:29#5)^main(void) {
  (297.c:30#2)^choose {
   -->
    (297.c:30#2)^guard(! (x_int32 ==_int32 0));
    (297.c:30#2)^ptr =(ptr) focus32 &(x);
   -->
    (297.c:30#2)^guard((x_int32 ==_int32 0));
    (297.c:30#2)^ptr =(ptr) focus32 &(x);
  }
}

ptr ptr;
int32 x;

