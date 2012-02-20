f : () -> ()
void (011.c:1#5)^f(void) {
  (011.c:3#8)^Array (Int) a;
  (011.c:4#4)^a + (64 : Int) =(int32) (1 : Int);
  (011.c:5#8)^{
    Int x;
    (011.c:5#8)^x =(int32) (a + (64 : Int)_Int : Int);
    (011.c:6#8)^{
      Int y;
      (011.c:6#8)^y =(int32) (a + (32 : Int)_Int : Int);
    }
  }
}

