Newspeak output
---------------
void (105.c:26#5)^main(void) {
  (105.c:27#7)^ptr ptr;
  (105.c:28#2)^choose {
   -->
    (105.c:28#2)^guard(! (ptr_ptr ==_ptr nil));
   -->
    (105.c:28#2)^guard((ptr_ptr ==_ptr nil));
  }
}


