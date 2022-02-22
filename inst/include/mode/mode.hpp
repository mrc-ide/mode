#pragma once
#include <vector>
namespace mode {
template <typename Model>
class solver {
private:
    const Model m_;
    double time_;
    std::vector<double> y_;
    const double dt_;
public:
    solver(Model m, double t0, std::vector<double> y0, double dt) : m_(m), time_(t0), y_(y0), dt_(dt) {
        if (y_.size() != m.size()) {
           cpp11::stop("y0 is not correct size for model: expected %d was %d", m.size(), y_.size());
        }
    }

    std::vector<double> solve(int num_steps) {
        auto dydt = std::vector<double>(y_.size());
        for (int i = 0; i < num_steps; ++i) {
            m_.rhs(time_, y_, dydt);
            time_ += dt_;
            for (size_t j = 0; j < y_.size(); ++j){
                y_[j] += dydt[j];
            }
        }
        return y_;
    }
};
}