#pragma once


#include <cpp11/external_pointer.hpp>
#include <cpp11/strings.hpp> // required to avoid link error only
#include <cpp11/list.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/protect.hpp>

#include <mode/mode.hpp>

namespace mode {
namespace r {

template <typename T>
cpp11::list mode_alloc(cpp11::list r_pars, double time, size_t n_particles) {
  auto pars = mode::mode_pars<T>(r_pars);
  container<T> *d = new mode::container<T>(pars, time, n_particles);
  cpp11::external_pointer<container<T>> ptr(d, true, false);
  return cpp11::writable::list({ptr});
}

template <typename T>
double mode_time(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->time();
}

cpp11::sexp state_array(const std::vector<double>& dat,
                        size_t n_state,
                        size_t n_particles) {
  cpp11::writable::doubles ret(dat.size());
  std::copy(dat.begin(), dat.end(), REAL(ret));

  ret.attr("dim") = cpp11::writable::integers{static_cast<int>(n_state),
                                              static_cast<int>(n_particles)};

  return ret;
}

template <typename T>
cpp11::sexp mode_solve(SEXP ptr, double end_time) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  auto time = obj->time();
  if (end_time < time) {
    cpp11::stop("end_time (%f) must be greater than current time (%f)",
                end_time, time);
  }
  obj->solve(end_time);

  std::vector<double> dat(obj->n_state() * obj->n_particles());
  obj->state(dat);
  return state_array(dat, obj->n_state(), obj->n_particles());
}

}
}
