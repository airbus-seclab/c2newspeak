f : () -> ()
void (006.c:1#5)^f(void) {
  (006.c:3#8)^_a3 x;
  (006.c:4#9)^Ptr (_a3) p;
  (006.c:4#9)^p =(ptr) (focus32 (&(x) : Ptr (_a3)) : Ptr (_a3));
}

g : () -> ()
void (006.c:7#5)^g(void) {
  (006.c:9#8)^Int x;
  (006.c:10#9)^Ptr (Int) p;
  (006.c:10#9)^p =(ptr) (focus32 (&(x) : Ptr (Int)) : Ptr (Int));
  (006.c:11#4)^x =(int32) (1 : Int);
}

h : () -> ()
void (006.c:14#5)^h(void) {
  (006.c:16#8)^Int x;
  (006.c:17#9)^Ptr (Int) p;
  (006.c:17#9)^p =(ptr) (focus32 (&(x) : Ptr (Int)) : Ptr (Int));
  (006.c:18#4)^[(p_Ptr (Int) : Ptr (Int))]32 =(int32) (1 : Int);
}

