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

int r_index_check(int x, int max) {
  if (x < 1 || x > max) {
    cpp11::stop("All elements of 'index' must lie in [1, %d]", max);
  }
  return x - 1;
}

cpp11::integers as_integer(cpp11::sexp x, const char * name) {
  if (TYPEOF(x) == INTSXP) {
    return cpp11::as_cpp<cpp11::integers>(x);
  } else if (TYPEOF(x) == REALSXP) {
    cpp11::doubles xn = cpp11::as_cpp<cpp11::doubles>(x);
    size_t len = xn.size();
    cpp11::writable::integers ret = cpp11::writable::integers(len);
    for (size_t i = 0; i < len; ++i) {
      double el = xn[i];
      if (!cpp11::is_convertable_without_loss_to_integer(el)) {
        cpp11::stop("All elements of '%s' must be integer-like",
                    name, i + 1);
      }
      ret[i] = static_cast<int>(el);
    }
    return ret;
  } else {
    cpp11::stop("Expected a numeric vector for '%s'", name);
    return cpp11::integers(); // never reached
  }
}

std::vector<size_t> r_index_to_index(cpp11::sexp r_index, size_t nmax) {
  cpp11::integers r_index_int = as_integer(r_index, "index");
  const int n = r_index_int.size();
  std::vector<size_t> index;
  index.reserve(n);
  for (int i = 0; i < n; ++i) {
    int x = r_index_check(r_index_int[i], nmax);
    index.push_back(x);
  }
  return index;
}

template <typename T>
void mode_set_index(SEXP ptr, cpp11::sexp r_index) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  const size_t index_max = obj->n_state_full();
  const std::vector<size_t> index = r_index_to_index(r_index, index_max);
  obj->set_index(index);
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

template <typename T>
cpp11::sexp mode_state_full(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  std::vector<double> dat(obj->n_state_full() * obj->n_particles());
  obj->state_full(dat);
  return state_array(dat, obj->n_state_full(), obj->n_particles());
}

template <typename T>
cpp11::sexp mode_stats(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  std::vector<size_t> dat(3 * obj->n_particles());
  obj->statistics(dat);
  return stats_array(dat, obj->n_particles());
}

std::vector<double> validate_time(cpp11::sexp r_time) {
  if (r_time == R_NilValue) {
    return std::vector<double>(0);
  }
  cpp11::doubles time = cpp11::as_cpp<cpp11::doubles>(r_time);
  if (time.size() != 1) {
    cpp11::stop("Expected 'time' to be a scalar value");
  }
  std::vector<double> ret(1);
  std::copy_n(REAL(time.data()), 1, ret.begin());
  return ret;
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

std::vector<double> validate_state(cpp11::sexp r_state,
                                   int n_state,
                                   int n_particles) {
  if (r_state == R_NilValue) {
    return std::vector<double>(0);
  }
  cpp11::doubles r_state_data = cpp11::as_cpp<cpp11::doubles>(r_state);
  const size_t state_len = r_state_data.size();
  auto dim = object_dimensions(r_state, n_state);
  if (dim.size() > 2) {
    cpp11::stop("Expected 'state' to have at most 2 dimensions");
  }
  if (dim.size() == 2) {
    if (dim[0] != n_state || dim[1] != n_particles) {
      cpp11::stop("Expected 'state' to be a %d by %d matrix but was %d by %d",
                  n_state, n_particles, dim[0], dim[1]);
    }
  }
  if (dim.size() == 1 && static_cast<int>(state_len) != n_state) {
    cpp11::stop("Expected 'state' to be a vector of length %d but was length %d",
                n_state, state_len);
  }
  std::vector<double> ret(state_len);
  std::copy_n(REAL(r_state_data.data()), state_len, ret.begin());
  return ret;
}

bool validate_set_initial_state(SEXP r_state, SEXP r_time,
                                SEXP r_set_initial_state) {
  bool set_initial_state = false;
  if (r_set_initial_state == R_NilValue) {
    set_initial_state = r_state == R_NilValue &&
                        r_time != R_NilValue;
  } else {
    set_initial_state = cpp11::as_cpp<bool>(r_set_initial_state);
    if (set_initial_state && r_state != R_NilValue) {
      cpp11::stop("'set_initial_state' cannot be TRUE unless 'state' is NULL");
    }
  }
  return set_initial_state;
}

bool validate_reset_step_size(SEXP r_time, SEXP r_reset_step_size) {
  bool reset_step_size = false;
  if (r_reset_step_size == R_NilValue) {
    reset_step_size = r_time != R_NilValue;
  } else {
    reset_step_size = cpp11::as_cpp<bool>(r_reset_step_size);
  }
  return reset_step_size;
}

template <typename T>
void mode_update_state(SEXP ptr, SEXP r_state, SEXP r_time,
                       SEXP r_set_initial_state,
                       SEXP r_reset_step_size) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();

  auto set_initial_state = validate_set_initial_state(r_state,
                                                      r_time,
                                                      r_set_initial_state);

  auto reset_step_size = validate_reset_step_size(r_time, r_reset_step_size);
  auto time = validate_time(r_time);
  auto state = validate_state(r_state,
                              static_cast<int>(obj->n_state_full()),
                              static_cast<int>(obj->n_particles()));
  obj->update_state(time, state, set_initial_state, reset_step_size);
}

}
}
