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
661.c
void main(void) {
  (661.c:27#6)^int32 x;
  (661.c:28#2)^choose {
   -->
    (661.c:28#2)^guard(! (0-_int32 ==_int32 0));
   -->
    (661.c:28#2)^guard((0-_int32 ==_int32 0));
  }
}

