Warning: the order of execution of side-effects in expressions not specified, picking a random one, be careful in 094.c line 28
Newspeak output
---------------
094.c
main() {
  (094.c:27#1072)^int32;
  (094.c:28#1077)^int32;
  (094.c:28#1077)^choose {
    | ! (0 > 1-_int32) -->
      (094.c:28#1077)^0- =(int32) (10 > 1-_int32);
    | (0 > 1-_int32) -->
      (094.c:28#1077)^0- =(int32) 0;
  }
  (094.c:28#1077)^1- =(int32) 0-_int32;
}


