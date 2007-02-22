// The coerce operator is also used for cast between integer of different 
// size or sign.
// Pointer creations are annotated by the size of the buffer they designate, 
// so as to allow invalid pointer operations checks.

void main() {
  int* x;
  int t[100];
  
  x = &t[3];
  x = x + 5;
  *x = 3;
}
