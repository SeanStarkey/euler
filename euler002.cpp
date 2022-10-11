
#include "fibonacci.cpp"
#include <iostream>

int euler002() {
  Fibonacci fib;

  int sum = 0;

  int index = 1;
  int fibValue = 0;
  while (fibValue < 4'000'000) {
    if (fibValue%2 == 0)
      sum += fibValue;
    fibValue = fib.get(index);
    index++;
  }
  return sum;
}

int main() {
  std::cout << euler002() << std::endl;
}
