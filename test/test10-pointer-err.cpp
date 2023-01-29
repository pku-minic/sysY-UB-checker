int var[10] = {0};
int var2[10] = {0};

int g(int m[], int i){
  m[i] = m[i] + 1;
  return m[i];
}

int f(int m[], int n[], int k){
  m[k] = g(n, k) + m[k];
  return m[k];
}

int recur(int i, int m[]){
  if(i > 0){
    var[i] = recur(i - 1, m) + m[i];
    return var[i];
  }else{
    return var[0];
  }
}

int main(){
  f(var, var2, 3);   // not a UB
  f(var, var, 3);   // UB
  recur(3, var);    // UB
  recur(3, var2);   // not a UB
  return 0;
}