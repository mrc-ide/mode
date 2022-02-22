#include <cpp11.hpp>
#include <mode/mode.hpp>

class logistic {
private:
    double r_;
    double K_;
public:
    logistic(double r, double K) : r_(r), K_(K) {
    }

    void rhs(double t, const std::vector<double>& y, std::vector<double>& dydt) const {
        const double N = y[0];
        dydt[0] = r_ * N * (1 - N / K_);
    }

    size_t size() const {
        return 1;
    }
};

[[cpp11::register]]
std::vector<double> solve_logistic(double r, double K, int end_time, std::vector<double> y0, double dt) {
    auto model = logistic(r, K);
    auto solver = mode::solver<logistic>(model, 0, y0, dt);
    auto ret = std::vector<double>(model.size() * (end_time + 1));
    auto it = ret.begin();
    it = std::copy_n(y0.begin(), model.size(), it);
    for(int i = 0; i < end_time; ++i) {
        auto y = solver.solve(1/dt);
        it = std::copy_n(y.begin(), model.size(), it);
    }
    return ret;
}
