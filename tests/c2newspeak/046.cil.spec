Newspeak output
---------------
046.c
void main(void) {
  (046.c:30#1132)^int32 x;
  (046.c:32#1140)^choose {
   -->
    (046.c:32#1140)^guard((0-_int32 ==_int32 1));
   -->
    (046.c:32#1140)^guard(! (0-_int32 ==_int32 1));
  }
}


