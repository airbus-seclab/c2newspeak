// Global and local variables declaration and access

// Global variables are designated by their name. 
int x;

void main() {
  // Variables are pushed on a stack, and local variables are accessed by 
  // their offset from the top of the stack
  int y;
  int z;
  
  x = y;
  x = z;
}
