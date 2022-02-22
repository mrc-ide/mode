#pragma once
namespace mode {
template <typename T>
class adder {
private:
    T base_;

public:
    adder(T base) : base_(base) {
    }

    T add(T x) {
        return base_ + x;
    }
};
}