Warning: 678.c:27#2: unknown identifier f, maybe a function without prototype accepted
Warning: 678.c:27#2: unknown arguments type at function call accepted
Newspeak output
---------------
678.c
void main(void) {
  (678.c:27#2)^int32 !tmp0;
  (678.c:27#2)^int32 f.arg1;
  (678.c:27#2)^0- =(int32) 1;
  (678.c:27#2)^f();
}


