class logistic {
public:
  using data_type = mode::no_data;
  using internal_type = mode::no_internal;
  using rng_state_type = dust::random::generator<double>;

  struct shared_type {
    double r1;
    double K1;
    double r2;
    double K2;
    double v;
  };

  logistic(const mode::pars_type<logistic>& pars): shared(pars.shared) {
  }

  void rhs(double t,
           const std::vector<double> &y,
           std::vector<double> &dydt) const {
    const double N1 = y[0];
    const double N2 = y[1];
    const double w  = y[2];
    dydt[0] = shared->r1 * N1 * (1 - N1 / (shared->K1 * w));
    dydt[1] = shared->r2 * N2 * (1 - N2 / (shared->K2 * w));

    // For now we just keep the stochastic component within the same
    // general vector, but it might be better to have a separate
    // argument here that holds the stochastic part (that does
    // complicate initialisation slightly, but not impossibly).
    dydt[2] = 0;
  }

  void update_stochastic(double t, std::vector<double>& y,
                         rng_state_type& rng_state) {
    const double r = dust::random::normal<double>(rng_state, 0, shared->v);
    y[2] *= std::exp(r);
  }

  std::vector<double> initial(double time) {
    std::vector<double> ret = {1, 1, 1};
    return ret;
  }

  size_t size() const {
    return 3;
  }

private:
  mode::shared_ptr<logistic> shared;
};

namespace mode {

template <>
mode::pars_type<logistic> mode_pars<logistic>(cpp11::list pars) {
  double r1 = cpp11::as_cpp<double>(pars["r1"]);
  double K1 = cpp11::as_cpp<double>(pars["K1"]);
  double r2 = cpp11::as_cpp<double>(pars["r2"]);
  double K2 = cpp11::as_cpp<double>(pars["K2"]);
  double v = cpp11::as_cpp<double>(pars["v"]);

  logistic::shared_type shared{r1, K1, r2, K2, v};
  return mode::pars_type<logistic>(shared);
}

}
