Newspeak output
---------------
void (450.c:26#5)^main(void) {
  (450.c:27#2)^choose {
   -->
    (450.c:27#2)^guard(! (focus16 &(cstr!0!.-) ==_ptr nil));
   -->
    (450.c:27#2)^guard((focus16 &(cstr!0!.-) ==_ptr nil));
  }
}

int8[2] cstr!0!.-;
(450.c:27#2)^cstr!0!.- =(int8) 45;
(450.c:27#2)^cstr!0!.- + 8 =(int8) 0;

