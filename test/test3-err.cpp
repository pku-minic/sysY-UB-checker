int var[10] = {7};

int f(int m[], int n[]){
  n[2] = m[1] + 1;
  return n[2];
}

int g(){
  int tmp[3] = {1, 2, 1};
  f(tmp, var);
  return 0;
}

int main(){
  var[2] = g();  // not a UB
  var[var[2]] = g();  // UB
  return 0;
}