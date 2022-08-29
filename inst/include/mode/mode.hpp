#pragma once

#ifdef _OPENMP
#include <omp.h>
#endif

#include <mode/solver.hpp>
#include <mode/types.hpp>

namespace mode {

// Models need to bring their own implementation here
template <typename T>
typename mode::pars_type<T> mode_pars(cpp11::list pars);

template <typename T>
cpp11::sexp mode_info(const mode::pars_type<T>& pars) {
  return R_NilValue;
}

// TODO: consider a better name here, but using the same name as the
// namespace does not end well...
template <typename T>
class container {
public:
  using model_type = T;
  using pars_type = mode::pars_type<T>;
  using rng_state_type = typename T::rng_state_type;
  using rng_int_type = typename rng_state_type::int_type;

  // Single parameter set case
  container(const pars_type &pars, const double time,
            const size_t n_particles, const size_t n_threads,
            const control ctl, const std::vector<rng_int_type>& seed)
    : n_pars_(0),
      n_particles_each_(n_particles),
      n_particles_total_(n_particles),
      pars_are_shared_(true),
      n_threads_(n_threads),
      m_({model_type(pars)}),
      rng_(n_particles_total_, seed, false),
      errors_(n_particles) {
    auto y = m_[0].initial(time);
    solver_.reserve(n_particles);
    for (size_t i = 0; i < n_particles; ++i) {
      solver_.push_back(solver<model_type>(m_[0], time, y, ctl));
    }
    initialise_index();
    shape_ = {n_particles};
  }

  // Multiple parameter set case
  container(const std::vector<pars_type>& pars, const double time,
            const size_t n_particles, const size_t n_threads,
            const control ctl, const std::vector<rng_int_type>& seed,
            const std::vector<size_t>& shape)
    : n_pars_(pars.size()),
      n_particles_each_(n_particles == 0 ? 1 : n_particles),
      n_particles_total_(n_particles_each_ * n_pars_),
      pars_are_shared_(n_particles != 0),
      n_threads_(n_threads),
      rng_(n_particles_total_, seed, false),
      errors_(n_particles_total_) {

    // TODO: This bit goes into a general initialise method, we'll
    // probably need to do this again with the parameter setting?

    // Make sure that all parameter sets create the same size state:
    size_t n = 0;
    const std::vector<model_type> m;
    for (size_t i = 0; i < n_pars_; ++i) {
      m.push_back(model_type(pars));
      const auto ni = m.back().size();
      if (i == 0) {
        n = ni;
      } else if (ni != n) {
        std::stringstream msg;
        msg << "'pars' created inconsistent state size: " <<
          "expected length " << n << " but parameter set " << i + 1 <<
          " created length " << ni;
        throw std::invalid_argument(msg.str());
      }
    }

    // on first creation:
    solver_.reserve(n_particles_total_);
    for (size_t i = 0; i < n_pars; ++i) {
      const auto y = m_[i].initial(time);
      for (size_t j = 0; j < n_particles; ++j) {
        solver_.push_back(solver<model_type>(m_[i], time, y, ctl));
      }
    }

    // on update:
    // for (size_t i = 0; i < n_particles_total_; ++i) {
    //   solver_[i].set_pars(pars[i / n_particles]);
    // }

    if (n_particles > 0) {
      shape_.push_back(n_particles);
    }
    for (auto i : shape) {
      shape_.push_back(i);
    }
  }

  control ctl() {
    return solver_[0].ctl();
  }

  size_t n_particles() {
    return n_particles_total_;
  }

  const std::vector<size_t>& shape() const {
    return shape_;
  }

  size_t n_state_full() {
    return m_[0].n_variables() + m_[0].n_output();
  }

  size_t n_state_run() const {
    return index_.size();
  }

  size_t n_variables() {
    return m_[0].n_variables();
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
    for (size_t i = 0; i < n_particles_total_; ++i) {
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

  void run(double end_time) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_total_; ++i) {
      try {
        solver_[i].solve(end_time, rng_.state(i));
      } catch (std::exception const& e) {
        errors_.capture(e, i);
      }
    }
    errors_.report();
  }

  std::vector<double> simulate(const std::vector<double>& end_time) {
    const size_t n_time = end_time.size();
    const size_t n_state = n_state_run();
    std::vector<double> ret(n_particles() * n_state * n_time);

#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles(); ++i) {
      try {
        for (size_t t = 0; t < n_time; ++t) {
          solver_[i].solve(end_time[t], rng_.state(i));
          size_t offset = t * n_state * n_particles() + i * n_state;
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
    for (size_t i = 0; i < n_particles_total_; ++i) {
      solver_[i].state(it + i * n_state_full());
    }
  }

  void state_run(std::vector<double> &end_state) {
    auto it = end_state.begin();
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_total_; ++i) {
      solver_[i].state(index_, it + i * n_state_run());
    }
  }

  void state(std::vector<double> &end_state,
             const std::vector<size_t>& index) {
    auto it = end_state.begin();
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_total_; ++i) {
      solver_[i].state(index, it + i * index.size());
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
      // TODO: probably needs update here...
      size_t n = index.size();
      const bool individual = state.size() == n * n_particles_total_;
      auto it = state.begin();
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
      for (size_t i = 0; i < n_particles_total_; ++i) {
        auto start = it;
        if (individual) {
            start = it + i * n;
        }
        solver_[i].set_state(t, start, index);
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    } else if (set_initial_state) {
      auto y = m_[0].initial(t);
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
      for (size_t i = 0; i < n_particles_total_; ++i) {
        solver_[i].set_state(t, y, index);
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    } else {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
      for (size_t i = 0; i < n_particles_total_; ++i) {
        solver_[i].set_time(t, reset_step_size);
        solver_[i].initialise(t);
      }
    }
  }

  // TODO: why does this not need set_state, dust does; there must be
  // some differently located logic which is a bit of a pity.
  // TODO: fix this for the multiparameter case
  // TODO: validate that we produce the correct size here in the
  // single parameter case, too.
  void set_pars(const pars_type& pars) {
    m_ = {model_type(pars)};
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_total_; ++i) {
      solver_[i].set_model(m_[0]);
    }
  }

  // TODO: reordering is more complicated in the multiparameter case,
  // as we need to interpret indicies within a block.
  void reorder(const std::vector<size_t>& index) {
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_total_; ++i) {
      size_t j = index[i];
      solver_[i].set_state(solver_[j]);
    }
#ifdef _OPENMP
#pragma omp parallel for schedule(static) num_threads(n_threads_)
#endif
    for (size_t i = 0; i < n_particles_total_; ++i) {
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
    for (size_t i = 0; i < n_particles_total_; ++i) {
      it = solver_[i].statistics(it);
    }
  }

  std::vector<std::vector<double>> debug_step_times() {
    std::vector<std::vector<double>> ret(n_particles_total_);
    // This could be in parallel safely
    for (size_t i = 0; i < n_particles_total_; ++i) {
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
  size_t n_pars_;
  size_t n_particles_each_;
  size_t n_particles_total_;
  bool pars_are_shared_;
  std::vector<size_t> shape_;
  size_t n_threads_;
  std::vector<model_type> m_;
  std::vector<size_t> index_;
  dust::random::prng<rng_state_type> rng_;
  dust::utils::openmp_errors errors_;
};

}
