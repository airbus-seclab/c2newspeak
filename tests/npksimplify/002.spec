main() {
  int4 stop;
  int4 i;
  int4 x;
  1- =(int4) 0;
  2- =(int4) (0 >= 10);
  choose {
    | ((0 >= 10) <> 0) -->
      0- =(int4) 1;
    | ((0 >= 10) == 0) -->
  }
}

