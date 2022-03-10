#pragma once
namespace mode {

struct control {
  // TODO: I've had to un-const these for a bit
  size_t max_steps;
  double atol;
  double rtol;
  double step_size_min;
  double step_size_max;
  double factor_safe = 0.9;
  double factor_min = 0.2;  // from dopri5.f:276, retard.f:328
  double factor_max = 10.0; // from dopri5.f:281, retard.f:333
  double beta = 0.04;
  double constant = 0.2 - 0.04 * 0.75; // 0.04 is beta

  control(size_t max_steps, double atol, double rtol, double step_size_min,
          double step_size_max) :
      max_steps(max_steps), atol(atol), rtol(rtol),
      step_size_min(step_size_min),
      step_size_max(step_size_max) {}

  control() : max_steps(10000), atol(1e-6), rtol(1e-6), step_size_min(1e-8),
              step_size_max(std::numeric_limits<double>::infinity()) {}
};

}
