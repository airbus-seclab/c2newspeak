Newspeak output
---------------
104.c
void main(void) {
  (104.c:27#1071)^ptr ptr;
  (104.c:28#1080)^choose {
   -->
    (104.c:28#1080)^guard(! (0-_ptr ==_ptr nil));
   -->
    (104.c:28#1080)^guard((0-_ptr ==_ptr nil));
  }
}


