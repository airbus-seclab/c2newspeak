int i;
void main() {
29:   i = 0;
28:   while (10 > i) {
31:     i = (i + 1);
      }
}

Analysis starts
29: i -> ? 
28: i -> 0 
31: i -> 0 
31: i -> ? 
31: potential invalid operation: +
Final state: i -> ? 
