Newspeak output
---------------
450.c
void main(void) {
  (450.c:27#1068)^choose {
   -->
    (450.c:27#1068)^guard(! (&_16(!450.c.const_str_-) ==_ptr nil));
   -->
    (450.c:27#1068)^guard((&_16(!450.c.const_str_-) ==_ptr nil));
  }
}

int8[2] !450.c.const_str_-;

