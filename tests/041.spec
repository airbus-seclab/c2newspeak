Warning: 'short' is not normalized: use 'short int' instead in 041.c line 31
Newspeak output
---------------
041.c
main() {
  (041.c:30#1133)^int8;
  (041.c:31#1144)^int16;
  (041.c:33#1152)^1- =(int8) coerce[-128,127] (1-_int8 + 0-_int16);
}


