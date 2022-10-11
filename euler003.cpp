
#include <iostream>
#include "timing.cpp"
#include "prime.cpp"

int euler003() {
  Prime prime(10000);
  auto factors = prime.factors(600851475143);
  return factors.back();
}

int main() {
  Timing timing;
  timing.start();
  std::cout << euler003() << std::endl;
  timing.end();
  std::cout << "Time: " << timing.duration() << " ms" << std::endl;
}
