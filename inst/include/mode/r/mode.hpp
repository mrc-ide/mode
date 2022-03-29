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

cpp11::sexp stats_array(const std::vector<size_t>& dat,
                        size_t n_particles) {
  cpp11::writable::integers ret(dat.size());
  std::copy(dat.begin(), dat.end(), ret.begin());
  ret.attr("dim") = cpp11::writable::integers{3, static_cast<int>(n_particles)};
  ret.attr("class") = "mode_statistics";
  auto row_names = cpp11::writable::strings{"n_steps",
                                            "n_steps_accepted",
                                            "n_steps_rejected"};
  ret.attr("dimnames") = cpp11::writable::list{row_names, R_NilValue};
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

template <typename T>
cpp11::sexp mode_stats(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  std::vector<size_t> dat(3 * obj->n_particles());
  obj->statistics(dat);
  return stats_array(dat, obj->n_particles());
}

double validate_time(cpp11::sexp r_time) {
  cpp11::doubles time = cpp11::as_cpp<cpp11::doubles>(r_time);
  if (time.size() != 1) {
    cpp11::stop("expected 'time' to be a scalar value");
  }
  return time[0];
}

template <typename T>
void mode_update_state(SEXP ptr, SEXP r_time) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  if (r_time != R_NilValue) {
    auto time = validate_time(r_time);
    obj->set_time(time);
  }
}

}
}
