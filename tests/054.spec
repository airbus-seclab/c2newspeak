Newspeak output
---------------
054.c
f() {
  (054.c:30#1131)^0- =(int8) 2;
}

main() {
  (054.c:34#1161)^int32;
  (054.c:35#1166)^int8;
  (054.c:35#1166)^0- =(int8) coerce[-128,127] 1-_int32;
  (054.c:35#1166)^f();
}


