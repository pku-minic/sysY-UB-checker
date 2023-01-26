int var = 1;

int f(int n){
  if(n == 0)return var;
  var = var + 1;
  return f(n - 1) + var;
}