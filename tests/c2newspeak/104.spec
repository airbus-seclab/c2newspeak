Newspeak output
---------------
104.c
void main(void) {
  (104.c:27#7)^ptr ptr;
  (104.c:28#2)^choose {
   -->
    (104.c:28#2)^guard(! (ptr_ptr ==_ptr nil));
   -->
    (104.c:28#2)^guard((ptr_ptr ==_ptr nil));
  }
}


