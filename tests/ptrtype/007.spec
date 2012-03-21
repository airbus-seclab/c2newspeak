f : () -> ()
void (007.c:6#5)^f(void) {
  (007.c:8#8)^Int x;
  (007.c:8#8)^x <- g();
}

g : () -> (Int)
Int (007.c:1#4)^g(void) {
  (007.c:3#4)^!return =(int32) (1 : Int);
}

