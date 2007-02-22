// Some fun with types and declaration
// Note that, since these variables are not used, C2Newspeak is run with
// option --keep-unused-vars, in order to keep them.

void main() {
  // Integer types are normalized according to their size and sign. 
  // Their size, which is architecture dependent, is made explicit.
  int i1;
  unsigned int i2;
  char i3;
  unsigned char i4;

  // Casts (and unions) in C allow programmers to manipulate sequences 
  // of bytes with any type. Consequently, Newspeak distinguishes only 
  // two types of pointers: data and function pointers.
  int *p1;
  unsigned int *p2;
  int (*p3)[10];
  struct { int x; } *p4;
  int (*fp)(int);

  // Newspeak composite data structures are arrays and regions. 
  // A region is a sequence of bytes. Some offsets in the region are 
  // indicated to store values of a given type. Regions can encode both 
  // C structures and unions, while making explicit their architecture 
  // dependent parameters: namely, fields' offsets, paddings and the 
  // overall type size.
  int t[10];
  struct { 
    int x; 
    char y; 
    char* z; 
  } s;
  union { 
    int x;
    char y;
    char* z; 
  } u;
  int t1[10][20];
  int t2[10][20][30];
  struct { 
    int x; 
    struct { char z; } y;
  } s1;
  struct { 
    int x[10]; 
    struct { char z[10]; } y[10];
  } s2;
  struct { 
    int z; 
    union { 
      int x; 
      char y; 
    } t; 
  } s3; 
}
