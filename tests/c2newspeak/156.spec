Debug: Parsing 156.c...
Debug: Parsing done.
Debug: Running first pass...
Debug: First pass done.
Debug: Translating 156.c...
Debug: 156.c:28#2: Translation done.
Debug: Writing 156.no...
Debug: Writing done.
Debug: Linking files...
Debug: Importing 156.no...
Debug: Importing done.
Debug: Globals...
Debug: Functions...
Debug: Importing funs from 156.no...
Debug: Funs import done.
Debug: Writing function: main
Debug: File linked.
Newspeak output
---------------
156.c
void main(void) {
  (156.c:27#6)^int32 x;
  (156.c:28#2)^0- =(int32) 3;
}


