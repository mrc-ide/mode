#include <utility>
#ifdef _OPENMP
#include <omp.h>
#endif

class parallel {
public:
  using data_type = mode::no_data;
  using internal_type = mode::no_internal;
  using rng_state_type = dust::random::generator<double>;

  struct shared_type {
    double sd;
  };

  parallel(const mode::pars_type<parallel>& pars) : shared(pars.shared) {
  }

  size_t n_variables() const {
    return 1;
  }

  size_t n_output() const {
    return 1;
  }

  std::vector<double> initial(double time) {
#ifdef _OPENMP
    static bool has_openmp = true;
#else
    static bool has_openmp = false;
#endif
    std::vector<double> ret = {(double) has_openmp};
    return ret;
  }

  void rhs(double t,
           const std::vector<double>& y,
           std::vector<double>& dydt) {
    dydt[0] = 0;
  }

  void output(double t,
              const std::vector<double>& y,
              std::vector<double>& output) {
#ifdef _OPENMP
    output[0] = (double) omp_get_thread_num();
#else
    output[0] = -1;
#endif
  }

  void update_stochastic(double t, const std::vector<double>& y,
                         std::vector<double>& y_next,
                         rng_state_type& rng_state) {
  }

private:
  mode::shared_ptr<parallel> shared;
};

namespace mode {
template <>
mode::pars_type<parallel> mode_pars<parallel>(cpp11::list pars) {
  double sd = cpp11::as_cpp<double>(pars["sd"]);
  parallel::shared_type shared{sd};
  return mode::pars_type<parallel>(shared);
}
}
