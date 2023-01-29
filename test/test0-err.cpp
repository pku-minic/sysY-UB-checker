int f(int x) {
  putint(x);
  return x;
}

void g(int x, int y) {}

int main() {
  g(f(1), f(2));      // UB
  g(getint(), f(2));  // not a UB
  return f(3) + f(4); // UB
}

