Warning: 464.c:33#2: dirty cast int32 -> fptr accepted
Newspeak output
---------------
464.c
void main(void) {
  (464.c:32#7)^fptr x;
  (464.c:33#2)^0- =(fptr) (fptr <= int32) 1;
}


