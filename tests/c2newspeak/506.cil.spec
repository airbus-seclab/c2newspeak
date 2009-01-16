Newspeak output
---------------
506.c
void main(void) {
  (506.c:32#1142)^choose {
   -->
    (506.c:32#1142)^guard(! (a_int32 ==_int32 0));
    (506.c:32#1142)^choose {
     -->
      (506.c:32#1142)^guard(! (b_int32 ==_int32 0));
     -->
      (506.c:32#1142)^guard((b_int32 ==_int32 0));
    }
   -->
    (506.c:32#1142)^guard((a_int32 ==_int32 0));
  }
}

int32 a = 0;
int32 b = 0;

