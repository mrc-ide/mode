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

  container(const pars_type& pars, const double time) {
    // This will get considerably more complicated once we move to
    // support multiple particles, and following the general approach
    // in dust will be sensible, I think.
    auto m = model_type(pars);
    auto y = m.initial(time);
    auto ctl = control();
    solver_.push_back(solver<model_type>(m, time, y, ctl));
  }

  double time() {
    return solver_[0].time();
  }

private:
  // NOTE: We're going to have a vector of solvers eventually, but
  // it's also quite hard to set this up to hold a single 'solver'
  // because otherwise we need to implement a default constructor for
  // the solver (something which we will then never use once we have
  // more than one system).
  std::vector<solver<model_type>> solver_;
};

}
