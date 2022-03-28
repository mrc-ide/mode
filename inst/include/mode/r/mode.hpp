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
    cpp11::stop("'end_time' (%f) must be greater than current time (%f)",
                end_time, time);
  }
  obj->solve(end_time);

  std::vector<double> dat(obj->n_state() * obj->n_particles());
  obj->state(dat);
  return state_array(dat, obj->n_state(), obj->n_particles());
}

double validate_time(cpp11::sexp r_time) {
  cpp11::doubles time = cpp11::as_cpp<cpp11::doubles>(r_time);
  if (time.size() != 1) {
    cpp11::stop("Expected 'time' to be a scalar value");
  }
  return time[0];
}

cpp11::integers object_dimensions(cpp11::sexp obj, size_t obj_size) {
  cpp11::integers dim;
  auto r_dim = obj.attr("dim");
  if (r_dim == R_NilValue) {
    dim = cpp11::writable::integers{static_cast<int>(obj_size)};
  } else {
    dim = cpp11::as_cpp<cpp11::integers>(r_dim);
  }
  return dim;
}

cpp11::doubles validate_state(cpp11::sexp r_state,
                              int n_state,
                              int n_particles) {
  cpp11::doubles state = cpp11::as_cpp<cpp11::doubles>(r_state);
  auto dim = object_dimensions(state, n_state);
  if (dim.size() > 2) {
    cpp11::stop("Expected 'state' to have at most 2 dimensions");
  }
  if (dim.size() == 2) {
    if (dim[0] != n_state || dim[1] != n_particles) {
      cpp11::stop("Expected 'state' to be a %d by %d matrix but was %d by %d",
                  n_state, n_particles, dim[0], dim[1]);
    }
  }
  if (dim.size() == 1 && state.size() != n_state) {
    cpp11::stop("Expected 'state' to be a vector of length %d but was length %d",
                n_state, state.size());
  }
  return state;
}

template <typename T>
void mode_update_state(SEXP ptr, SEXP r_state, SEXP r_time) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  if (r_time != R_NilValue) {
    auto time = validate_time(r_time);
    obj->set_time(time);
  }
  if (r_state != R_NilValue) {
    auto state = validate_state(r_state,
                                static_cast<int>(obj->n_state()),
                                static_cast<int>(obj->n_particles()));
   // obj->set_state(state);
  }
}

}
}
