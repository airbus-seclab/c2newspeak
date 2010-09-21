int buffer[100];
int head;
int tail;

int isFull()
{
  return (head == tail);
}

int isEmpty()
{
  return (head == tail + 1) || (tail == 0 && head == 99);
}

void put()
{
    if (!isFull())
    {
      buffer[tail] = 42;
      tail++;
      if (tail == 100) tail = 0;  // 100 becomes 101
    }
}

void get()
{
  if (!isEmpty())
    {
      int v = buffer[head];
      head++;
      if (head == 100) head = 0;
      // return v;
    }
  //  return -1;
}

void main() 
{
  head = 0;
  tail = 1;
  
  while (1)
    {
      int rnd;
      if (rnd) put();
      else get();
    }
}
