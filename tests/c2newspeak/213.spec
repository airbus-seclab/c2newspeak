Newspeak output
---------------
213.c
void main(void) {
  (213.c:30#4)^int32 !tmp0;
  (213.c:30#4)^f();
  (213.c:28#5)^do {
    (213.c:29#2)^choose {
     -->
      (213.c:29#2)^guard(! (0-_int32 ==_int32 0));
      (213.c:30#4)^goto lbl0;
     -->
      (213.c:29#2)^guard((0-_int32 ==_int32 0));
    }
  } with lbl0: {
  }
}


