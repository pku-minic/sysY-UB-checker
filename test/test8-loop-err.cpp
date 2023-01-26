int var[10] = {0};

int f(int m[], int k) {
  m[3] = m[2] + k;
  return m[3];
}

int g(int h[], int k, int j) {
  int q = k + j;
  return f(h, h[3]);
}

int mm(int h[], int k, int j) {
  int q = k * j;
  return g(h, q, h[3]);
}

int main() {
  var[1] = mm(var, var[2], var[3]);  // not a UB
  for (int i = mm(var, 1, 2) + mm(var, 1, 3); i + var[2] < mm(var, 3, 4);
       i = i + var[0] + mm(var, 1, 2)) {  // UB, UB, UB
    var[var[2]] = mm(var, 2, 3);          // UB;
    var[3] = mm(var, mm(var, 2, 3), 1);
  }
  while(var[7] > mm(var, 1, 2)){    // UB
    var[3] = mm(var, mm(var, 2, 3), mm(var, 2, 3));   // UB
  }
}
