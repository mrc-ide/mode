#pragma once

#include <mode/solver.hpp>
#include <mode/types.hpp>

namespace mode {

// Models need to bring their own implementation here
template <typename T>
typename mode::pars_type<T> mode_pars(cpp11::list pars);

// TODO: consider a better name here, but using the same name as the
// namespace does not end well...
template <typename T>
class container {
public:
  using model_type = T;
  using pars_type = mode::pars_type<T>;

  container(const pars_type &pars, const double time,
            const size_t n_particles) : n_particles_(n_particles),
                                        m_(model_type(pars)) {
    auto ctl = control();
    auto y = m_.initial(time);
    for (size_t i = 0; i < n_particles; ++i) {
      solver_.push_back(solver<model_type>(m_, time, y, ctl));
    }
  }

  size_t n_particles() {
    return n_particles_;
  }

  size_t n_state() {
    return solver_[0].size();
  }

  double time() {
    return solver_[0].time();
  }

  void solve(int end_time) {
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].solve(end_time);
    }
  }

  void state(std::vector<double> &end_state) {
    auto it = end_state.begin();
    for (size_t i = 0; i < n_particles_; ++i) {
      it = solver_[i].state(it);
    }
  }

  void update_state(std::vector<double> time,
                    const std::vector<double>& state,
                    bool set_initial_state,
                    bool reset_step_size) {
    auto t = solver_[0].time();
    if (time.size() > 0) {
      t = time[0];
    }
    if (state.size() > 0) {
      const bool individual = state.size() == n_state() * n_particles_;
      auto it = state.begin();
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_time(t);
        solver_[i].set_state(t, it);
        if (reset_step_size) {
          solver_[i].set_initial_step_size();
        }
        if (individual) {
          it += n_state();
        }
      }
    } else if (set_initial_state) {
      auto y = m_.initial(t);
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_time(t);
        solver_[i].set_state(t, y);
        if (reset_step_size) {
          solver_[i].set_initial_step_size();
        }
      }
    } else {
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_time(t);
        if (reset_step_size) {
          solver_[i].set_initial_step_size();
        }
      }
    }
  }

  void statistics(std::vector<size_t> &all_stats) {
    auto it = all_stats.begin();
    for (size_t i = 0; i < n_particles_; ++i) {
      it = solver_[i].statistics(it);
    }
  }

private:
  std::vector<solver<model_type>> solver_;
  size_t n_particles_;
  model_type m_;
};

}
