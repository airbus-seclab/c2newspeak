Fun __builtin_strchr;
Fun __builtin_strcmp;
Fun __builtin_strncpy;
Fun __builtin_strncat;
Fun __builtin_expect;
void main() {
  int32 x;
  if (!(x == 0)) {
    {
    }
  } else {
  }
}
Newspeak output
---------------
void (661.c:26#5)^main(void) {
  (661.c:27#6)^int32 x;
  (661.c:28#2)^choose {
   -->
    (661.c:28#2)^guard(! (x_int32 ==_int32 0));
   -->
    (661.c:28#2)^guard((x_int32 ==_int32 0));
  }
}


