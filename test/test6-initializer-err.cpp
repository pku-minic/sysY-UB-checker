int var = 1;
int v2[5] = {0};

int f(int a) {
  var = var + a;
  return var;
}

int g(int v[], int i, int m) {
  v[i] = v[i] + m;
  return v[i];
}

int main() {
  int tmp[4][3] = {f(var), g(v2, 0, 0)};            // not a UB
  int tmp2[4][3] = {f(f(var)), g(v2, 1, 1)};        // not a UB
  int tmp3[4][3] = {{g(v2, 2, g(v2, 1, f(var)))}};  // not a UB
  int tmp4[4][3] = {1, 2, f(var), {f(3)}};          // UB
  int tmp5[4][3] = {f(var), g(v2, 0, 0),
                    g(v2, 1, 4)};           // not a UB but will give a warning
  int tmp6[4][3] = {g(v2, 1, f(3)), f(4)};  // UB
}
