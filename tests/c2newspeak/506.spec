Newspeak output
---------------
506.c
void main(void) {
  (506.c:32#2)^choose {
   -->
    (506.c:32#2)^guard(! (a_int32 ==_int32 0));
    (506.c:32#2)^guard(! (b_int32 ==_int32 0));
   -->
    (506.c:32#2)^choose {
     -->
      (506.c:32#2)^guard(! (a_int32 ==_int32 0));
      (506.c:32#2)^guard((b_int32 ==_int32 0));
     -->
      (506.c:32#2)^guard((a_int32 ==_int32 0));
    }
  }
}

int32 a;
int32 b;

