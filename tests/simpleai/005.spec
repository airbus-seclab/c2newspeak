int i;
void main() {
29:   i = 0;
30:   while (10 > i) {
31:     i = (i + 1);
      }
}

Analysis starts
29: i -> ? 
30: i -> 0 
31: i -> 0 
31: i -> ? 
31: potential invalid operation: +
Final state: i -> ? 
