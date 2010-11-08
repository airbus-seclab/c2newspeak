Warning: 771.c:10#0: goto statement accepted
Newspeak output
---------------
771.c
void main(void) {
  (771.c:2#6)^uint32 goto!again;
  (771.c:2#6)^goto!again =(uint32) 0;
  (771.c:2#6)^{
    int32 x;
    (771.c:3#6)^int32 y;
    (771.c:5#2)^do {
      (771.c:5#2)^while (1) {
        (771.c:6#2)^do {
          (771.c:8#4)^do {
            (771.c:6#2)^choose {
             -->
              (771.c:6#2)^guard((x_int32 ==_int32 1));
              (771.c:8#4)^goto lbl5;
             -->
              (771.c:6#2)^guard(! (x_int32 ==_int32 1));
              (771.c:6#2)^goto lbl4;
            }
          } with lbl5:
          (771.c:9#6)^choose {
           -->
            (771.c:9#6)^guard(! (y_int32 ==_int32 0));
            (771.c:10#1)^goto!again =(uint32) 1;
           -->
            (771.c:9#6)^guard((y_int32 ==_int32 0));
          }
          (771.c:10#1)^choose {
           -->
            (771.c:10#1)^guard(goto!again_int32);
            (771.c:10#1)^goto lbl4;
           -->
            (771.c:10#1)^guard(! goto!again_int32);
          }
        } with lbl4:
        (771.c:5#2)^choose {
         -->
          (771.c:5#2)^guard(goto!again_int32);
         -->
          (771.c:5#2)^guard(! goto!again_int32);
          (771.c:5#2)^goto lbl1;
        }
      }
    } with lbl1:
  }
}


