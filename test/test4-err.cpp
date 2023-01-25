int var[10] = {0};

int g(int v[]){
  return getarray(v);
}
int f(){
  return var[4];
}

int main(){
  int a = g(var) + var[5], b = a + 1;   // UB
  a = a + f() + g(var);     // UB
}
