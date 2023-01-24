#pragma once

#ifdef _OPENMP
#include <omp.h>
#endif

#include <dust/types.hpp> // dust::pars_type
#include <mode/solver.hpp>

namespace mode {

// TODO: consider a better name here, but using the same name as the
// namespace does not end well...
template <typename T>
class dust_ode {
public:
  using model_type = T;
  using time_type = double;
  using real_type = typename T::real_type;
  using data_type = typename T::data_type;
  using pars_type = dust::pars_type<T>;
  using rng_state_type = typename T::rng_state_type;
  using rng_int_type = typename rng_state_type::int_type;

  dust_ode(const pars_type &pars, const double time,
           const size_t n_particles, const size_t n_threads,
           const control ctl, const std::vector<rng_int_type>& seed)
      : n_particles_(n_particles),
        n_threads_(n_threads),
        shape_({n_particles}),
        m_(model_type(pars)),
        rng_(n_particles_, seed, false),
        errors_(n_particles){
    auto y = m_.initial(time);
    for (size_t i = 0; i < n_particles; ++i) {
      solver_.push_back(solver<model_type>(m_, time, y, ctl));
    }
    initialise_index();
  }

  control ctl() {
    return solver_[0].ctl();
  }

  size_t n_particles() {
    return n_particles_;
  }

  size_t n_state_full() const {
    return m_.n_variables() + m_.n_output();
  }

  size_t n_state() const {
    return index_.size();
  }

  size_t n_variables() {
    return m_.n_variables();
  }

  // Until we support multiple parameter sets, this is always zero
  // (i.e., what dust uses when pars_multi = FALSE)
  size_t n_pars() const {
    return 0;
  }

  size_t n_pars_effective() const {
    return 1;
  }

  size_t pars_are_shared() const {
    return true;
  }

  void set_index(const std::vector<size_t>& index) {
    index_ = index;
  }

  void set_n_threads(size_t n_threads) {
    n_threads_ = n_threads;
  }

  void set_stochastic_schedule(const std::vector<double>& time) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].set_stochastic_schedule(time);
    }
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

  const std::vector<size_t>& shape() const {
    return shape_;
  }

  void run(double time_end) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      try {
        solver_[i].solve(time_end, rng_.state(i));
      } catch (std::exception const& e) {
        errors_.capture(e, i);
      }
    }
    errors_.report();
  }

  std::vector<double> simulate(const std::vector<double>& time_end) {
    const size_t n_time = time_end.size();
    std::vector<double> ret(n_particles() * n_state() * n_time);

#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles(); ++i) {
      try {
        for (size_t t = 0; t < n_time; ++t) {
          solver_[i].solve(time_end[t], rng_.state(i));
          size_t offset = t * n_state() * n_particles() + i * n_state();
          solver_[i].state(index_, ret.begin() + offset);
        }
      } catch (std::exception const& e) {
        errors_.capture(e, i);
      }
    }
    errors_.report();
    return ret;
  }

  void state_full(std::vector<double> &end_state) {
    auto it = end_state.begin();
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].state(it + i * n_state_full());
    }
  }

  void state(std::vector<double> &end_state) {
    auto it = end_state.begin();
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].state(index_, it + i * n_state());
    }
  }

  void state(const std::vector<size_t>& index,
             std::vector<double> &end_state) {
    auto it = end_state.begin();
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].state(index, it + i * index.size());
    }
  }

  void set_time(double time) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].set_time(time, false);
    }
  }

  void set_state(const std::vector<real_type>& state,
                 const std::vector<size_t>& index) {
    const bool use_index = index.size() > 0;
    const size_t n_state = use_index ? index.size() : n_state_full();
    const bool individual = state.size() == n_state * n_particles_;
    const size_t n = individual ? 1 : n_particles_; // really n_particles_each_
    auto it = state.begin();
    const auto t = time();

#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      // in dust we use it_i = it + (i / n) * n_state
      auto start = it;
      if (individual) {
        start = it + i * n;
      }
      solver_[i].set_state(t, start, index);
    }
  }

  void initialise(bool reset_step_size) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      if (reset_step_size) {
        solver_[i].set_initial_step_size;
      }
      solver_[i].initialise();
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
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
      for (size_t i = 0; i < n_particles_; ++i) {
        auto start = it;
        if (individual) {
            start = it + i * n;
        }
        solver_[i].set_state(t, start, index);
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    } else if (set_initial_state) {
      auto y = m_.initial(t);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_state(t, y, index);
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    } else {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    }
  }

  void set_pars(const pars_type& pars, bool set_initial_state) {
    m_ = model_type(pars);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].set_model(m_);
    }

    if (set_initial_state) {
      const auto t = solver_[0].time();
      auto y = m_.initial(t);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
      for (size_t i = 0; i < n_particles_; ++i) {
        solver_[i].set_state(t, y);
        solver_[i].initialise(t);
      }
    }
  }

  void reorder(const std::vector<size_t>& index) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      size_t j = index[i];
      solver_[i].set_state(solver_[j]);
    }
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_; ++i) {
      solver_[i].swap();
    }
  }

  void statistics(std::vector<size_t> &all_stats) {
    auto it = all_stats.begin();
    // this is hard to make parallel safe without doing
    //
    // solver_i[i].statistics(it + i * 3);
    //
    // which requires knowing that we always have three statistics
    // (though we do rely on this in r/mode.hpp::mode_stats)
    for (size_t i = 0; i < n_particles_; ++i) {
      it = solver_[i].statistics(it);
    }
  }

  std::vector<std::vector<double>> debug_step_times() {
    std::vector<std::vector<double>> ret(n_particles_);
    // This could be in parallel safely
    for (size_t i = 0; i < n_particles_; ++i) {
      ret[i] = solver_[i].debug_step_times();
    }
    return ret;
  }

  void check_errors() {
    if (errors_.unresolved()) {
      throw std::runtime_error("Errors pending; reset required");
    }
  }

  void reset_errors() {
    errors_.reset();
  }

  std::vector<typename rng_state_type::int_type> rng_state() {
    return rng_.export_state();
  }

  void set_rng_state(const std::vector<typename rng_state_type::int_type>& rng_state) {
    rng_.import_state(rng_state);
  }

private:
  std::vector<solver<model_type>> solver_;
  size_t n_particles_;
  size_t n_threads_;
  std::vector<size_t> shape_;
  model_type m_;
  std::vector<size_t> index_;
  dust::random::prng<rng_state_type> rng_;
  dust::utils::openmp_errors errors_;
};

}
