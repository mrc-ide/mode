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

  container(const pars_type& pars, const double time,
            const size_t n_particles): n_particles_(n_particles) {
    // This will get considerably more complicated once we move to
    // support multiple particles, and following the general approach
    // in dust will be sensible, I think.
    auto m = model_type(pars);
    auto y = m.initial(time);
    auto ctl = control();
    for (size_t i; i < n_particles; ++i){
      solver_.push_back(solver<model_type>(m, time, y, ctl));
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
    for (size_t i; i < n_particles_; ++i){
      solver_[i].solve(end_time);
    }
  }

  void state(std::vector<double>& end_state) {
    auto it = end_state.begin();
    for (size_t i = 0; i < n_particles_; ++i) {
      it = solver_[i].state(it);
    }
  }

private:
  // NOTE: We're going to have a vector of solvers eventually, but
  // it's also quite hard to set this up to hold a single 'solver'
  // because otherwise we need to implement a default constructor for
  // the solver (something which we will then never use once we have
  // more than one system).
  std::vector<solver<model_type>> solver_;
  size_t n_particles_;
};

}
