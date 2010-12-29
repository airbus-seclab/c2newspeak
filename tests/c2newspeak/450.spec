Newspeak output
---------------
void (450.c:26#5)^main(void) {
  (450.c:27#2)^choose {
   -->
    (450.c:27#2)^guard(! (focus16 &(cstr!0!450.c.-) ==_ptr nil));
   -->
    (450.c:27#2)^guard((focus16 &(cstr!0!450.c.-) ==_ptr nil));
  }
}

int8[2] cstr!0!450.c.-;
(450.c:27#2)^cstr!0!450.c.- =(int8) 45;
(450.c:27#2)^cstr!0!450.c.- + 8 =(int8) 0;

