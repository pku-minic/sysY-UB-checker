int f(int x) {
  putint(x);
  return x;
}

void g(int x, int y) {}

int main() {
  g(f(1), f(2));      // 可能输出 12 或 21
  return f(3) + f(4); // 可能输出 34 或 43
}

