Newspeak output
---------------
void main(void) {
  (450.c:27#2)^choose {
   -->
    (450.c:27#2)^guard(! (focus16 &(cstr!1!450.c.-) ==_ptr nil));
   -->
    (450.c:27#2)^guard((focus16 &(cstr!1!450.c.-) ==_ptr nil));
  }
}

int8[2] cstr!1!450.c.-;
(450.c:27#2)^cstr!1!450.c.- =(int8) 45;
(450.c:27#2)^cstr!1!450.c.- + 8 =(int8) 0;

