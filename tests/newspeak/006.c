// when visiting the expression in the condition it is important to pass 
// the location of the condition, and not of the last instruction in the block
// of the preceding branch
void main() {
  int i;
  if (i) {
    i = 1;
  }
}
