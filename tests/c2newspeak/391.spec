Newspeak output
---------------
391.c
main() {
  (391.c:29#2)^x =(int32) -1;
  (391.c:30#2)^choose {
    | (4 > coerce[0,4294967295] x_int32) -->
      (391.c:32#4)^x =(int32) 1;
    | ! (4 > coerce[0,4294967295] x_int32) -->
  }
}

int32 x = 0;

