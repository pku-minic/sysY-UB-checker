int a = 5;

int f() {
  a = a + 1;
  return a;
}
int g() {
  a = a + 2;
  return a;
}

int main() {
  int b[10] = {0};
  for (int i = 0; i < 10; i = i + 1) {
    b[i] = f() + g();
  }
  return 0;
}