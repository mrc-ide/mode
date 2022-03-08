#include <cpp11.hpp>
#include <mode/solver.hpp>

class logistic {
private:
  double r1_;
  double K1_;
  double r2_;
  double K2_;
public:
  logistic(double r1, double K1, double r2, double K2) :
      r1_(r1), K1_(K1), r2_(r2), K2_(K2) {
  }

  void rhs(double t,
           const std::vector<double> &y,
           std::vector<double> &dydt) const {
    const double N1 = y[0];
    const double N2 = y[1];
    dydt[0] = r1_ * N1 * (1 - N1 / K1_);
    dydt[1] = r2_ * N2 * (1 - N2 / K2_);
  }

  size_t size() const {
    return 2;
  }
};

[[cpp11::register]]
std::vector<double>
solve_logistic(cpp11::doubles r, cpp11::doubles K, int end_time,
               std::vector<double> y0) {
  auto model = logistic(r[0], K[0], r[1], K[1]);
  auto ctl = mode::control();
  auto solver = mode::solver<logistic>(model, 0, y0, ctl);
  auto ret = std::vector<double>(model.size() * (end_time + 1));
  auto it = ret.begin();
  it = std::copy_n(y0.begin(), model.size(), it);
  for (int i = 0; i < end_time; ++i) {
    auto y = solver.solve(i + 1);
    it = std::copy_n(y.begin(), model.size(), it);
  }
  return ret;
}
