Warning: 464.c:33#2: dirty cast int32 -> fptr accepted
Newspeak output
---------------
void main(void) {
  (464.c:32#7)^fptr x;
  (464.c:33#2)^x =(fptr) (fptr <= int32) 1;
}


