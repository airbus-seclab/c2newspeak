Newspeak output
---------------
372.c
f() {
}

main() {
  (372.c:31#6)^int32 a;
  (372.c:31#9)^int32 b;
  (372.c:32#2)^do {
    (372.c:32#2)^choose {
      | ! (1-_int32 ==_int32 0) -->
        (372.c:32#2)^goto lbl4;
      | (1-_int32 ==_int32 0) -->
        (372.c:32#2)^choose {
          | ! (0-_int32 ==_int32 0) -->
            (372.c:32#2)^goto lbl4;
          | (0-_int32 ==_int32 0) -->
        }
    }
  } with lbl4: {
    (372.c:33#4)^f();
  }
}


