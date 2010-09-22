int a[10];
int *p;
int x;

void main (void) {
  p = &a[0];
  p += 5;     // OK
  x = *p;     // OK
  p += 10;    // Alarm
  x = *p;     // Alarm
}
