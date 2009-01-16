Newspeak output
---------------
105.c
void main(void) {
  (105.c:27#7)^ptr ptr;
  (105.c:28#2)^choose {
   -->
    (105.c:28#2)^guard(! (0-_ptr ==_ptr nil));
   -->
    (105.c:28#2)^guard((0-_ptr ==_ptr nil));
  }
}


