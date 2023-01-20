int a = 5;

int f(){
  a = a + 1;
  return a;
}

int main(){
  int b[10] = {0};
  b[a] = f();
  return 0;
}