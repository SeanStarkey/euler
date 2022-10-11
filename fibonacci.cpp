/*

Compute fibonacci numbers.

*/
#include <unordered_map>

class Fibonacci {
private:
  std::unordered_map<int, long long> memo;
  
public:
long long get(const int term) {
  if (term == 1)
    return 1;
  if (term == 2)
    return 2;
  if (memo.contains(term))
    return memo[term];

  long long result = get(term-1) + get(term-2);
  memo[term] = result;
  return result;
}
  
};


