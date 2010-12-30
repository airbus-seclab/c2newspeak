// objective of the test:
// even if string "hello" is present in both files, there should be only
// one global created for it at the end

void f()
{
  char *x;
  x = "hello";
}
