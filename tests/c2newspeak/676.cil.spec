Newspeak output
---------------
676.c
void main(void) {
  (676.c:31#1096)^choose {
   -->
    (676.c:31#1096)^guard(! (a_int32 ==_int32 0));
    (676.c:32#1121)^x =(int32) 1;
   -->
    (676.c:31#1096)^guard((a_int32 ==_int32 0));
    (676.c:31#1096)^choose {
     -->
      (676.c:31#1096)^guard(! (b_int32 ==_int32 0));
      (676.c:32#1121)^x =(int32) 1;
     -->
      (676.c:31#1096)^guard((b_int32 ==_int32 0));
      (676.c:31#1096)^choose {
       -->
        (676.c:31#1096)^guard(! (c_int32 ==_int32 0));
        (676.c:32#1121)^x =(int32) 1;
       -->
        (676.c:31#1096)^guard((c_int32 ==_int32 0));
      }
    }
  }
}

int32 a;
int32 b;
int32 c;
int32 x;

