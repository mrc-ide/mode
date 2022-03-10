#pragma once

#include <cmath>

#include "utils.hpp"

namespace mode {
namespace {
constexpr double C2 = 0.2;
constexpr double C3 = 0.3;
constexpr double C4 = 0.8;
constexpr double C5 = 8.0 / 9.0;
constexpr double A21 = 0.2;
constexpr double A31 = 3.0 / 40.0;
constexpr double A32 = 9.0 / 40.0;
constexpr double A41 = 44.0 / 45.0;
constexpr double A42 = -56.0 / 15.0;
constexpr double A43 = 32.0 / 9.0;
constexpr double A51 = 19372.0 / 6561.0;
constexpr double A52 = -25360.0 / 2187.0;
constexpr double A53 = 64448.0 / 6561.0;
constexpr double A54 = -212.0 / 729.0;
constexpr double A61 = 9017.0 / 3168.0;
constexpr double A62 = -355.0 / 33.0;
constexpr double A63 = 46732.0 / 5247.0;
constexpr double A64 = 49.0 / 176.0;
constexpr double A65 = -5103.0 / 18656.0;
constexpr double A71 = 35.0 / 384.0;
constexpr double A73 = 500.0 / 1113.0;
constexpr double A74 = 125.0 / 192.0;
constexpr double A75 = -2187.0 / 6784.0;
constexpr double A76 = 11.0 / 84.0;
constexpr double E1 = 71.0 / 57600.0;
constexpr double E3 = -71.0 / 16695.0;
constexpr double E4 = 71.0 / 1920.0;
constexpr double E5 = -17253.0 / 339200.0;
constexpr double E6 = 22.0 / 525.0;
constexpr double E7 = -1.0 / 40.0;
// ---- DENSE OUTPUT OF SHAMPINE (1986)
constexpr double D1 = -12715105075.0 / 11282082432.0;
constexpr double D3 = 87487479700.0 / 32700410799.0;
constexpr double D4 = -10690763975.0 / 1880347072.0;
constexpr double D5 = 701980252875.0 / 199316789632.0;
constexpr double D6 = -1453857185.0 / 822651844.0;
constexpr double D7 = 69997945.0 / 29380423.0;
}

template<typename Model>
class stepper {
private:
  Model m;
  size_t n;
  std::vector<double> y;
  std::vector<double> y_next;
  std::vector<double> y_stiff;
  std::vector<double> k1;
  std::vector<double> k2;
  std::vector<double> k3;
  std::vector<double> k4;
  std::vector<double> k5;
  std::vector<double> k6;

public:

  stepper(Model m) : m(m), n(m.size()), y(n), y_next(n), y_stiff(n), k1(n),
                     k2(n), k3(n), k4(n), k5(n), k6(n) {}

  void step(double t, double h) {

    for (size_t i = 0; i < n; ++i) { // 22
      y_next[i] = y[i] + h * A21 * k1[i];
    }
    m.rhs(t + C2 * h, y_next, k2);
    for (size_t i = 0; i < n; ++i) { // 23
      y_next[i] = y[i] + h * (A31 * k1[i] + A32 * k2[i]);
    }
    m.rhs(t + C3 * h, y_next, k3);
    for (size_t i = 0; i < n; ++i) { // 24
      y_next[i] = y[i] + h * (A41 * k1[i] + A42 * k2[i] + A43 * k3[i]);
    }
    m.rhs(t + C4 * h, y_next, k4);
    for (size_t i = 0; i < n; ++i) { // 25
      y_next[i] = y[i] + h * (A51 * k1[i] + A52 * k2[i] + A53 * k3[i] +
                              A54 * k4[i]);
    }
    m.rhs(t + C5 * h, y_next, k5);
    for (size_t i = 0; i < n; ++i) { // 26
      y_stiff[i] = y[i] + h * (A61 * k1[i] + A62 * k2[i] +
                               A63 * k3[i] + A64 * k4[i] +
                               A65 * k5[i]);
    }
    const double t_next = t + h;
    m.rhs(t_next, y_stiff, k6);
    for (size_t i = 0; i < n; ++i) { // 27
      y_next[i] = y[i] + h * (A71 * k1[i] + A73 * k3[i] + A74 * k4[i] +
                              A75 * k5[i] + A76 * k6[i]);
    }
    m.rhs(t_next, y_next, k2);

    for (size_t i = 0; i < n; ++i) {
      k4[i] = h * (E1 * k1[i] + E3 * k3[i] + E4 * k4[i] +
                   E5 * k5[i] + E6 * k6[i] + E7 * k2[i]);
    }

  }

  double error(double atol, double rtol) {
    double err = 0.0;
    for (size_t i = 0; i < n; ++i) {
      const double sk = atol + rtol *
                               std::max(std::abs(y[i]), std::abs(y_next[i]));
      err += square(k4[i] / sk);
    }
    return std::sqrt(err / n);
  }

  void reset(double t, std::vector<double> y0) {
    for (size_t i = 0; i < n; ++i) {
      y[i] = y0[i];
    }
    m.rhs(t, y, k1);
  }

  void step_complete(double t, double h) {
    std::copy_n(k2.begin(), n, k1.begin()); // k1 = k2
    std::copy_n(y_next.begin(), n, y.begin()); // y = y_next
  }

  const std::vector<double>& output() const {
    return y;
  }

};

}
