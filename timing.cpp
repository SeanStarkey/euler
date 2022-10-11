#include <chrono>

class Timing {
public:
  void start() {
    startTime = std::chrono::steady_clock::now();
  }
  
  void end() {
    endTime = std::chrono::steady_clock::now();
  }
  
  int duration() {
    return std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime).count();
  }

private:
  std::chrono::steady_clock::time_point startTime;
  std::chrono::steady_clock::time_point endTime;
};
