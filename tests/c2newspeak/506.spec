Newspeak output
---------------
506.c
void main(void) {
  (506.c:32#2)^choose {
    | ! (a_int32 ==_int32 0) -->
      (506.c:32#2)^choose {
        | ! (b_int32 ==_int32 0) -->
        | (b_int32 ==_int32 0) -->
      }
    | (a_int32 ==_int32 0) -->
  }
}

int32 a = 0;
int32 b = 0;

