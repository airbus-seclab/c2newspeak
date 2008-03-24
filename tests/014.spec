Warning: 'long long' is not standard: use 'long long int' instead in 014.c line 33
Newspeak output
---------------
014.c
main() {
  (014.c:33#1242)^uint64;
  (014.c:34#1247)^0- =(uint64) 9223372036854775807;
  (014.c:35#1277)^0- =(uint64) 9223372036854775808;
}


