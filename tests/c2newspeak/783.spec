Warning: 783.c:2#0: 'long signed' is not normalized: use 'signed long int' instead
Warning: 783.c:3#0: 'long signed int' is not normalized: use 'signed long int' instead
Warning: 783.c:4#0: 'long unsigned' is not normalized: use 'unsigned long int' instead
Warning: 783.c:5#0: 'long unsigned int' is not normalized: use 'unsigned long int' instead
Warning: 783.c:7#0: 'short signed' is not normalized: use 'signed short int' instead
Warning: 783.c:8#0: 'short signed int' is not normalized: use 'signed short int' instead
Warning: 783.c:9#0: 'short unsigned' is not normalized: use 'unsigned short int' instead
Warning: 783.c:10#0: 'short unsigned int' is not normalized: use 'unsigned short int' instead
Newspeak output
---------------
void (783.c:1#5)^main(void) {
  (783.c:2#14)^int32 l1;
  (783.c:3#18)^int32 l2;
  (783.c:4#16)^uint32 l3;
  (783.c:5#20)^uint32 l4;
  (783.c:7#15)^int16 s1;
  (783.c:8#19)^int16 s2;
  (783.c:9#17)^uint16 s3;
  (783.c:10#21)^uint16 s4;
}


