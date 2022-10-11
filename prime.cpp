#include <iostream>
#include <vector>
#include <set>

class Prime {
public:
  Prime(int populate) : sieve(populate, Uninitialized) {
    sieve[0] = NotPrimeNumber;
    sieve[1] = NotPrimeNumber;
    for (int i=2; i<populate; i++) {
      if (sieve[i] == Uninitialized) {
	sieve[i] = PrimeNumber;
	primeSet.insert(i);
	for (int j=i+i; j<populate; j+=i) {
	  sieve[j] = NotPrimeNumber;
	}
      }
    }
  }

  bool isPrime(int potentialPrime) {
    return sieve[potentialPrime] == PrimeNumber;
  }

  void iteratePrimes() {
    for (int p : primeSet) {
      std::cout << p << std::endl;
    }
  }
  
  std::vector<int> factors(long long number) {
    std::vector<int> results;
    for (int p : primeSet) {
      while (number%p == 0) {
	results.push_back(p);
	number = number / p;
      }
      if (number == 1)
	break;
    }
    return results;
  }

private:
  enum SieveStatus {
	       Uninitialized,
	       PrimeNumber,
	       NotPrimeNumber
  };
  
  std::vector<SieveStatus> sieve;
  std::set<int> primeSet;
};
