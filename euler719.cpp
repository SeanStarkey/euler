
#include <iostream>
#include "timing.cpp"

bool isSnumberHelper(long sum, long number, long target) {
    if (sum > target)
        return false;

    if (sum+number == target)
        return true;
    long len = 1;
    long numberLen = number;
    while (numberLen > 0) {
        len++;
        numberLen = numberLen/10;
    }
    bool result = false;
    for (int i=1; i<len; i++) {
        long div = 1;
        for (int j=1; j<i; j++)
            div = div * 10;
        long first = number / div;
        long second = number % div;
        //std::cout << number << " " << first << " " << second << std::endl;
        result = result || isSnumberHelper(sum + first, second, target);
    }
    return result;
}

long euler719() {
    long numsqrt = 1;
    long num = numsqrt*numsqrt;
    long sum = 0;
    while (num < 1'000'000'000'000l) {
        //while (num < 10'000l) {
        if (numsqrt % 10000 == 0)
            std::cout << num << " " << numsqrt << std::endl;
        numsqrt++;
        num = numsqrt*numsqrt;
        //std::cout << "---" << std::endl;
        //std::cout << numsqrt << " " << num << std::endl;
        if (isSnumberHelper(0, num, numsqrt)) {
            sum += num;
            //std::cout << numsqrt << " " << num << " " << sum << std::endl;
        }
    }
    return sum;
}

int main() {
  Timing timing;
  timing.start();
  std::cout << euler719() << std::endl;
  //std::cout << isSnumberHelper(0, 418646526841, 647029) << std::endl;
  timing.end();
  std::cout << "Time: " << timing.duration()/1000.0/1000.0 << " s" << std::endl;
}
