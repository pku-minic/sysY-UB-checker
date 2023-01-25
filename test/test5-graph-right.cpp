int var[10] = {4, 7, 2};
int mmm = 9;

int g(int m[], int i){
	return m[i] * 2;
}

int f(int v){
	return v + 1 + var[0];
}

int main(){
	var[f(mmm)] = g(var, f(g(var, mmm + 1))); // not a UB
	return 0;
}
