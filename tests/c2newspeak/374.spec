Newspeak output
---------------
374.c
main() {
  (374.c:29#1)^choose {
    | ! (y_int32 ==_int32 0) -->
      (374.c:29#1)^choose {
        | ! (z_int32 ==_int32 0) -->
          (374.c:29#1)^x =(int32) 1;
        | (z_int32 ==_int32 0) -->
          (374.c:29#1)^choose {
            | ! (t_int32 ==_int32 0) -->
              (374.c:29#1)^x =(int32) ! (u_int32 ==_int32 0);
            | (t_int32 ==_int32 0) -->
              (374.c:29#1)^x =(int32) 0;
          }
      }
    | (y_int32 ==_int32 0) -->
      (374.c:29#1)^choose {
        | ! (t_int32 ==_int32 0) -->
          (374.c:29#1)^x =(int32) ! (u_int32 ==_int32 0);
        | (t_int32 ==_int32 0) -->
          (374.c:29#1)^x =(int32) 0;
      }
  }
}

int32 t = 0;
int32 u = 0;
int32 x = 0;
int32 y = 0;
int32 z = 0;

