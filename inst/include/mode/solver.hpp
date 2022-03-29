#pragma once

#include <vector>

#include "control.hpp"
#include "initial_step_size.hpp"
#include "stats.hpp"
#include "stepper.hpp"

namespace mode {
template<typename Model>
class solver {
private:
  Model m_;
  double t_;
  double h_;
  control ctl_;
  stats stats_;
  double last_error_;
  stepper<Model> stepper_;
  size_t size_;
public:
  solver(Model m, double t, std::vector<double> y, control ctl) : m_(m),
                                                                  t_(t),
                                                                  ctl_(ctl),
                                                                  stepper_(m),
                                                                  size_(
                                                                      m.size()) {
    reset(t, y);
  }

  double time() const {
    return t_;
  }

  double step(double tcrit) {
    bool success = false;
    bool reject = false;
    const double fac_old = std::max(last_error_, 1e-4);

    double h = h_;
    while (!success) {
      if (stats_.n_steps > ctl_.max_steps) {
        throw std::runtime_error("too many steps");
      }
      if (h < ctl_.step_size_min) {
        throw std::runtime_error("step too small");
      }
      if (h <= std::abs(t_) * std::numeric_limits<double>::epsilon()) {
        throw std::runtime_error("step size vanished");
      }
      if (t_ + h > tcrit) {
        h = tcrit - t_;
      }

      // Carry out the step
      stepper_.step(t_, h);
      stats_.n_steps++;

      // Error estimation
      const auto err = stepper_.error(ctl_.atol, ctl_.rtol);

      const double fac11 = std::pow(err, ctl_.constant);
      const double facc1 = 1.0 / ctl_.factor_min;
      const double facc2 = 1.0 / ctl_.factor_max;

      if (err <= 1) {
        success = true;
        stats_.n_steps_accepted++;

        stepper_.step_complete(t_, h);

        double fac = fac11 / std::pow(fac_old, ctl_.beta);
        fac = clamp(fac / ctl_.factor_safe,
                    facc2, facc1);
        const double h_new = h / fac;

        t_ += h;
        if (reject) {
          h_ = std::min(h_new, h);
        } else {
          h_ = std::min(h_new, ctl_.step_size_max);
        }
        last_error_ = err;
      } else {
        reject = true;
        if (stats_.n_steps_accepted >= 1) {
          stats_.n_steps_rejected++;
        }
        h /= std::min(facc1, fac11 / ctl_.factor_safe);
      }
    }
    return t_;
  }

  void solve(int t) {
    while (t_ < t) {
      step(t);
    }
  }

  void reset(double t, std::vector<double> y) {
    stepper_.reset(t, y);
    stats_.reset();
    h_ = initial_step_size(m_, t, y, ctl_);
    t_ = t;
  }

  size_t size() {
    return size_;
  }

  std::vector<double>::iterator
  state(std::vector<double>::iterator end_state) const {
    for (size_t i = 0; i < size_; ++i, end_state++) {
      *end_state = stepper_.output()[i];
    }
    return end_state;
  }

  std::vector<size_t>::iterator
  statistics(std::vector<size_t>::iterator all_stats) const {
    *all_stats = stats_.n_steps;
    all_stats++;
    *all_stats = stats_.n_steps_accepted;
    all_stats++;
    *all_stats = stats_.n_steps_rejected;
    all_stats++;
    return all_stats;
  }
};
}