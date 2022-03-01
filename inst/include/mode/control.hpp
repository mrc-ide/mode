#pragma once
namespace mode {

struct control {
  const size_t max_steps;
  const double atol;
  const double rtol;
  const double step_size_min;
  const double step_size_max;
  const double factorSafe = 0.9;
  const double factorMin = 0.2;  // from dopri5.f:276, retard.f:328
  const double factorMax = 10.0; // from dopri5.f:281, retard.f:333
  const double beta = 0.04;
  const double constant = 0.2 - 0.04 * 0.75; // 0.04 is beta
};

}
