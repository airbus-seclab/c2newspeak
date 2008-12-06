Newspeak output
---------------
391.c
void main(void) {
  (391.c:29#1076)^x =(int32) -1;
  (391.c:30#1086)^choose {
    | (4 > coerce[0,4294967295] x_int32) -->
      (391.c:32#1139)^x =(int32) 1;
    | ! (4 > coerce[0,4294967295] x_int32) -->
  }
}

int32 x = 0;

