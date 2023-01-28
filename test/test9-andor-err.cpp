int f = 9;
int var[10] = {0};

int main() {
  while (getch() && getint()) {  // not a UB
    if (getarray(var) || var[7]) {   // not a UB
      return 0;
    } else {
      return getint() + getarray(var);   // UB
    }
  }
}