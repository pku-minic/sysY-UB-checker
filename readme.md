## SysY-UB-Checker

在定义 SysY 语言规范时并没有考虑到 SysY 中表达式求值顺序影响最终结果的情况。但实际上这是可能发生的，例如

```c
int f(int x) {
  putint(x);
  return x;
}

void g(int x, int y) {}

int main() {
  g(f(1), f(2));      // 可能输出 12 或 21
  return f(3) + f(4); // 可能输出 34 或 43
}
```

因此我们需要一个静态的检测器，来检测测试用例中是否存在相应的UB。

### 0. Test

`test`目录下是一些包含UB的测试用例。`run.sh`是一个测试课程在线测试用例是否包含UB的脚本。`./run.sh`会产生一个`result.txt`文件，包含了所有含有UB的测试用例。

### 1. Overview

首先阐述该 Checker 检测的UB的具体定义：在 SysY 语句中，对同一块内存地址的修改与读取，修改与修改之间没有没有先后顺序则为UB（在上面的例子中，我们假想`putint`修改了一个全局变量`__globalOutput__`，类似的`getch`等函数修改了全局变量`__globalInput__`）。

我们不能期望静态的UB检测器能够准确地检查出上述UB，例如：

```c
int var[10] = {0};

int f(int v[], int i){
    v[i] = v[i] + 1;
    return v[i];
}

int main(){
    var[4] = f(var, getint()) + var[4]; // UB ?
    return 0;
}
```

考虑到该 Checker 的目的，我们将其设计得保守一些：**如果该 Checker 没有检测出UB，那么该程序一定不存在上述定义的UB，如果该 Checker 检测出UB，该程序可能存在上述定义的UB**

### 2. Implementation

SysY 的求值顺序规定为与 C 语言一致，因此 SysY 中存在下述的 Sequence Point

* 一个以分号结尾的表达式与另一个以分号结尾的表达式之间的内存访问是有序的
* 变量声明时逗号分隔的初始化表达式的内存访问是有序的（例如，`int a=f(), b=f();`）
* 逻辑运算符`||`, `&&`的前后操作数的求值的内存访问是有序的（短路求值）
* 在函数调用语句中，函数体中的内存访问与函数参数求值的内存访问是有序的
* 在赋值语句中，存储赋值结果的内存访问与操作数求值之间的内存访问是有序的，因此，`var[2] = var[2] + 1`不是UB，: )

在实现时，我们利用 Clang 解析出的语法树，确定每个分号结尾的表达式访问了哪些内存，根据上面的 Sequence Point 的定义确定这些内存访问的序关系，从而检测出UB。**为了解决上面提到的数组访问导致的动态UB的问题，在实现上我们假设每一次数组访问会访问该数组对应的所有内存。**

### 3. Possible Improvement

* 削弱每一次数组访问会访问该数组对应的所有内存的假设
* 使用 Clang 的 `DiagnosticEngine`来发射UB警告
