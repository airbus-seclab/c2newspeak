Newspeak output
---------------
void (450.c:26#5)^main(void) {
  (450.c:27#2)^choose {
   -->
    (450.c:27#2)^guard(! (focus16 &(!cstr.-) ==_ptr nil));
   -->
    (450.c:27#2)^guard((focus16 &(!cstr.-) ==_ptr nil));
  }
}

int8[2] !cstr.-;
(450.c:27#2)^!cstr.- =(int8) 45;
(450.c:27#2)^!cstr.- + 8 =(int8) 0;

