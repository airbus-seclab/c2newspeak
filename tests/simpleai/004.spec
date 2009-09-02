int x;
void main() {
29:   x = 1;
30:   if (10 > x) {
30:     x = (x + 1);
      } else {
      }
}

Analysis starts
29: x -> [-2147483648, 2147483647] 
30: x -> 1 
30: x -> 1 
Final state: x -> 2 
