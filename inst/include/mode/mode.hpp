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
    initialise_index();
  }

  size_t n_particles() {
    return n_particles_;
  }

  size_t n_state_full() {
    return solver_[0].size();
  }

  size_t n_state() const {
    return index_.size();
  }

  void set_index(const std::vector<size_t>& index) {
    index_ = index;
  }

  void initialise_index() {
    const size_t n = n_state_full();
    index_.clear();
    index_.reserve(n);
    for (size_t i = 0; i < n; ++i) {
      index_.push_back(i);
    }
  }

  double time() {
    return solver_[0].time();
  }

  void run(double end_time) {
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].solve(end_time);
    }
  }

  void state_full(std::vector<double> &end_state) {
    auto it = end_state.begin();
    for (size_t i = 0; i < n_particles_; ++i) {
      it = solver_[i].state_full(it);
    }
  }

  void state(std::vector<double> &end_state) {
    auto it = end_state.begin();
    for (size_t i = 0; i < n_particles_; ++i) {
      it = solver_[i].state(index_, it);
    }
  }

  void state(std::vector<double> &end_state, const std::vector<size_t>& index) {
    auto it = end_state.begin();
    for (size_t i = 0; i < n_particles_; ++i) {
      it = solver_[i].state(index, it);
    }
  }

  void update_state(std::vector<double> time,
                    const std::vector<double>& state,
                    const std::vector<size_t>& index,
                    bool set_initial_state,
                    bool reset_step_size) {
    auto t = solver_[0].time();
    if (time.size() > 0) {
      t = time[0];
    }
    if (state.size() > 0) {
      size_t n = index.size();
      const bool individual = state.size() == n * n_particles_;
      auto it = state.begin();
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_state(t, it, index);
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
        if (individual) {
          it += n;
        }
      }
    } else if (set_initial_state) {
      auto y = m_.initial(t);
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_state(t, y, index);
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    } else {
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    }
  }

  void set_pars(const pars_type& pars) {
    m_ = model_type(pars);
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].set_model(m_);
    }
  }

  void reorder(const std::vector<size_t>& index) {
    for (size_t i = 0; i < n_particles_; ++i) {
      size_t j = index[i];
      solver_[i].set_state(solver_[j]);
    }
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].swap();
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
  std::vector<size_t> index_;
};

}
