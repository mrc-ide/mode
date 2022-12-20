#include <utility>
#ifdef _OPENMP
#include <omp.h>
#endif

class parallel {
public:
  using real_type = double;
  using data_type = mode::no_data;
  using internal_type = mode::no_internal;
  using rng_state_type = dust::random::generator<real_type>;

  struct shared_type {
    real_type sd;
  };

  parallel(const mode::pars_type<parallel>& pars) : shared(pars.shared) {
  }

  size_t n_variables() const {
    return 1;
  }

  size_t n_output() const {
    return 1;
  }

  std::vector<real_type> initial(real_type time) {
#ifdef _OPENMP
    static bool has_openmp = true;
#else
    static bool has_openmp = false;
#endif
    std::vector<real_type> ret = {(real_type) has_openmp};
    return ret;
  }

  void rhs(real_type t,
           const std::vector<real_type>& y,
           std::vector<real_type>& dydt) {
    dydt[0] = 0;
  }

  void output(real_type t,
              const std::vector<real_type>& y,
              std::vector<real_type>& output) {
#ifdef _OPENMP
    output[0] = (real_type) omp_get_thread_num();
#else
    output[0] = -1;
#endif
  }

  void update_stochastic(real_type t, const std::vector<real_type>& y,
                         rng_state_type& rng_state,
                         std::vector<real_type>& y_next) {
  }

private:
  mode::shared_ptr<parallel> shared;
};

namespace mode {
template <>
mode::pars_type<parallel> mode_pars<parallel>(cpp11::list pars) {
  parallel::real_type sd = cpp11::as_cpp<double>(pars["sd"]);
  parallel::shared_type shared{sd};
  return mode::pars_type<parallel>(shared);
}
}
