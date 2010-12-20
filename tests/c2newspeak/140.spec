Warning: 140.c:32#2: dirty cast from integer to pointer accepted
Warning: 140.c:32#2: dirty cast from integer to pointer accepted
Newspeak output
---------------
void main(void) {
  (140.c:30#6)^int32 x;
  (140.c:31#7)^ptr ptr;
  (140.c:32#2)^choose {
   -->
    (140.c:32#2)^guard(((ptr) x_int32 ==_ptr ptr_ptr));
   -->
    (140.c:32#2)^guard(! ((ptr) x_int32 ==_ptr ptr_ptr));
  }
}


