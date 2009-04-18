Newspeak output
---------------
676.c
void main(void) {
  (676.c:31#2)^choose {
   -->
    (676.c:31#2)^choose {
     -->
      (676.c:31#2)^guard(! (a_int32 ==_int32 0));
     -->
      (676.c:31#2)^guard((a_int32 ==_int32 0));
      (676.c:31#2)^choose {
       -->
        (676.c:31#2)^guard(! (b_int32 ==_int32 0));
       -->
        (676.c:31#2)^guard((b_int32 ==_int32 0));
        (676.c:31#2)^guard(! (c_int32 ==_int32 0));
      }
    }
    (676.c:32#4)^x =(int32) 1;
   -->
    (676.c:31#2)^guard((a_int32 ==_int32 0));
    (676.c:31#2)^guard((b_int32 ==_int32 0));
    (676.c:31#2)^guard((c_int32 ==_int32 0));
  }
}

int32 a = 0;
int32 b = 0;
int32 c = 0;
int32 x = 0;

