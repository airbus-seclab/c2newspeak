Newspeak output
---------------
111.c
void main(void) {
  (111.c:27#6)^int32 x;
  (111.c:27#9)^int32 y;
  (111.c:28#2)^choose {
   -->
    (111.c:28#2)^guard(! (1-_int32 ==_int32 0));
    (111.c:28#2)^choose {
     -->
      (111.c:28#2)^guard(! (0-_int32 ==_int32 0));
     -->
      (111.c:28#2)^guard((0-_int32 ==_int32 0));
    }
   -->
    (111.c:28#2)^guard((1-_int32 ==_int32 0));
  }
}


