Newspeak output
---------------
450.c
void main(void) {
  (450.c:27#2)^choose {
   -->
    (450.c:27#2)^guard(! (focus16 &(!450.c.const_str_-) ==_ptr nil));
   -->
    (450.c:27#2)^guard((focus16 &(!450.c.const_str_-) ==_ptr nil));
  }
}

int8[2] !450.c.const_str_-;
(450.c:27#2)^!450.c.const_str_- =(int8) 45;
(450.c:27#2)^!450.c.const_str_- + 8 =(int8) 0;

