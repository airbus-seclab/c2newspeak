int main() {
  char t[100] = "Hello";
  char witness[10];
  
  if (t[20] == 0) {
    // should signal this alarm
    witness[100] = 1;
  } else {
    // should not signal this alarm
    witness[100] = 1;
  }
  return 0;
}
