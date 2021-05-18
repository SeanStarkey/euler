#include <iostream>

int euler001()
{
    int sum = 0;
    for (int i3=3; i3<1000; i3+=3) {
        sum += i3;
    }
    for (int i5=5; i5<1000; i5+=5) {
        if (i5%3 != 0)
            sum += i5;
    }
    return sum;
}


int main() {
    std::cout << euler001() << std::endl;
}
