
int x;

void onecpy(char* dst, char* src) {
  *dst = *src;
}

int main() {
  int *p1;
  int *p2;
  
  onecpy((char*)&p1, (char*)&p2);
  
  // should be a warning here
  // what is the value of p2 here? should be corrupted
  // in Venet's analyses will say p2 points to x and not signal any warning
  // Venet's analyses only check pointer out of bounds (not invalid pointers)
  *p1 = 1;
  
  return 0;
}
