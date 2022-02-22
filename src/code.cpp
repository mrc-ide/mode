#include <cpp11.hpp>
#include <mode/mode.hpp>

template <typename T>
T add(T a, T b) {
    auto object = mode::adder<T>(a);
    return object.add(b);
}

[[cpp11::register]]
int add_int(int a, int b) {
    return add(a, b);
}
