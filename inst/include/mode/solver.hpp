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
  size_t n_output_;
  double h_swap_;
  double last_error_swap_;
  stats stats_swap_;
  std::vector<double> state_full_;
public:
  solver(Model m,
         double t,
         std::vector<double> y,
         control ctl) : m_(m),
                        t_(t),
                        ctl_(ctl),
                        stepper_(m),
                        size_(m.size()),
                        n_output_(m.n_output()),
                        state_full_(size_ + n_output_) {
    stats_.reset();
    set_state(t, y);
    set_initial_step_size();
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

  void solve(double t) {
    while (t_ < t) {
      step(t);
    }
  }

  void set_time(double t, bool reset_step_size) {
    if (t != t_) {
      stats_.reset();
      t_ = t;
    }
    if (reset_step_size) {
      set_initial_step_size();
    }
  }

  void set_initial_step_size() {
    h_ = initial_step_size(m_, t_, stepper_.state(), ctl_);
  }

  void set_state(double t,
                 std::vector<double>::const_iterator state) {
    stepper_.set_state(t, state);
    stepper_.initialise(t);
  }

  void set_state(double t,
                 std::vector<double>::const_iterator state,
                 const std::vector<size_t>& index) {
    stepper_.set_state(t, state, index);
    stepper_.initialise(t);
  }

  void initialise(double t) {
    stepper_.initialise(t);
  }

  void set_state(double t,
                 const std::vector<double> &state) {
    set_state(t, state.begin());
  }

  void set_state(double t,
                 const std::vector<double> &state,
                 const std::vector<size_t>& index) {
    set_state(t, state.begin(), index);
  }

  void set_state(const solver<Model>& other) {
    stepper_.set_state(other.stepper_);
    h_swap_ = other.h_;
    last_error_swap_ = other.last_error_;
    stats_swap_ = other.stats_;
  }

  void swap() {
    stepper_.swap();
    h_ = h_swap_;
    last_error_ = last_error_swap_;
    stats_ = stats_swap_;
  }

  void set_model(Model m) {
    m_ = m;
    stepper_.set_model(m);
  }

  size_t size() {
    return size_;
  }

  std::vector<double>::iterator
  state(const std::vector<size_t>& index,
             std::vector<double>::iterator end_state) {
    auto it = state_full_.begin();
    stepper_.state(it);
    m_.output(t_, it);
    for (size_t i = 0; i < index.size(); ++i, end_state++) {
        *end_state = state_full_[index[i]];
    }
    return end_state;
  }

  std::vector<double>::iterator
  state_full(std::vector<double>::iterator end_state) {
    stepper_.state(end_state);
    end_state = m_.output(t_, end_state);
    return end_state;
  }

  std::vector<size_t>::iterator
  statistics(std::vector<size_t>::iterator all_stats) const {
    all_stats[0] = stats_.n_steps;
    all_stats[1] = stats_.n_steps_accepted;
    all_stats[2] = stats_.n_steps_rejected;
    return all_stats + 3;
  }
};
}
