/* The GCD algorithm in MicroC */
int a;
int b;
img v;

int gcd(int a, int b) {
  while (a != b) {
    if (b < a) a = a - b;
    else b = b - a;
  }
  return a;
}

int main() {
  int x;
  int y;
  int z;
  int s;
  a = 18;
  b = 9;
  x = 2;
  y = 14;
  z = 6 mod 5;
  print(gcd(x,y));
  print(gcd(3,15));
  print(gcd(99,121));
  print(gcd(a,b));
  print(z);
  return 0;
}
