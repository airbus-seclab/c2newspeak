Newspeak output
---------------
105.c
void main(void) {
  (105.c:27#1071)^ptr ptr;
  (105.c:28#1080)^choose {
   -->
    (105.c:28#1080)^guard(! (0-_ptr ==_ptr nil));
   -->
    (105.c:28#1080)^guard((0-_ptr ==_ptr nil));
  }
}


