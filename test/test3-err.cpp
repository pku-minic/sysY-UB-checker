int var[10] = {7, 4};
int var2[10][10] = {0};

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
	var2[f(var,var)][f(var, var2[1])] = 3;  // not a UB, but give a warning
	var2[f(var, var)][f(var2[1], var)] = 3;  // UB
  return 0;
}
