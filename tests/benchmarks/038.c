int main() {
  int i;
  int t[12];
  
  i = 0;
  do {
    // should not signal any warnings
    t[i] = 0;
    i++;
  } while (i < 12);
}
