// Function calls have no arguments and look like assembly calls. 
// Newspeak takes advantage of its stack to transmit parameters.

int f(int a, int b) {
  return a + b;
}

void main() {
  int x, y, z;
  z = f(x, y);
}
