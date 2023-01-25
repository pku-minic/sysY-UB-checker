int var[10] = {7, 4};

int f(int m[], int n[]){
  n[2] = m[1] + 1;
  return n[2];
}

int g(){
  int tmp[3] = {1, 2, 1}, tmpb = 1 + var[2], tmpc;
  f(tmp, var);
  return 0;
}

int main(){
  var[2] = g();  // not a UB
  var[var[2]] = g();  // UB
  return 0;
}
