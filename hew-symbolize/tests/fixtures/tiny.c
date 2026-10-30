static int level3(int n) { return n * 3; }
int level2(int n) { return level3(n + 1) + 1; }
int main(void) { return level2(2); }
