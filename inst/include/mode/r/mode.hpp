#pragma once


#include <cpp11/external_pointer.hpp>
#include <cpp11/strings.hpp> // required to avoid link error only
#include <cpp11/list.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/protect.hpp>

#include <mode/mode.hpp>
#include <mode/r/helpers.hpp>

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

template <typename T>
void mode_set_index(SEXP ptr, cpp11::sexp r_index) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  if (r_index == R_NilValue) {
    obj->initialise_index();
  } else {
    const size_t index_max = obj->n_state_full();
    const std::vector <size_t> index =
        mode::r::r_index_to_index(r_index, index_max);
    obj->set_index(index);
  }
}

template <typename T>
cpp11::sexp mode_run(SEXP ptr, double end_time) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  auto time = obj->time();
  if (end_time < time) {
    cpp11::stop("'end_time' (%f) must be greater than current time (%f)",
                end_time, time);
  }
  obj->run(end_time);

  std::vector<double> dat(obj->n_state() * obj->n_particles());
  obj->state(dat);
  return mode::r::state_array(dat, obj->n_state(), obj->n_particles());
}

template <typename T>
cpp11::sexp mode_state_full(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  std::vector<double> dat(obj->n_state_full() * obj->n_particles());
  obj->state_full(dat);
  return mode::r::state_array(dat, obj->n_state_full(), obj->n_particles());
}

template <typename T>
cpp11::sexp mode_state(SEXP ptr, SEXP r_index) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  const size_t index_max = obj->n_state_full();
  const std::vector <size_t> index =
      mode::r::r_index_to_index(r_index, index_max);
  size_t n = index.size();
  std::vector<double> dat(n * obj->n_particles());
  obj->state(dat, index);
  return mode::r::state_array(dat, n, obj->n_particles());
}

template <typename T>
cpp11::sexp mode_stats(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  std::vector<size_t> dat(3 * obj->n_particles());
  obj->statistics(dat);
  return mode::r::stats_array(dat, obj->n_particles());
}

template <typename T>
void mode_update_state(SEXP ptr, SEXP r_pars, SEXP r_state, SEXP r_time,
                       SEXP r_set_initial_state,
                       SEXP r_reset_step_size) {
  mode::container<T> *obj =
      cpp11::as_cpp < cpp11::external_pointer<mode::container<T>>>(ptr).get();

  auto set_initial_state = mode::r::validate_set_initial_state(r_state,
                                                               r_pars,
                                                               r_time,
                                                               r_set_initial_state);

  auto reset_step_size = mode::r::validate_reset_step_size(r_time,
                                                           r_pars,
                                                           r_reset_step_size);
  auto time = mode::r::validate_time(r_time);
  auto state = mode::r::validate_state(r_state,
                                       static_cast<int>(obj->n_state_full()),
                                       static_cast<int>(obj->n_particles()));
  if (r_pars != R_NilValue) {
    auto pars = mode::mode_pars<T>(r_pars);
    obj->set_pars(pars);
  }
  obj->update_state(time, state, set_initial_state, reset_step_size);
}

template <typename T>
size_t mode_n_state(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->n_state();
}

template <typename T>
size_t mode_n_state_full(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->n_state_full();
}

template <typename T>
void mode_reorder(SEXP ptr, cpp11::sexp r_index) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  const size_t index_max = obj->n_particles();
  const std::vector <size_t> index =
      mode::r::r_index_to_index(r_index, index_max);
  if (index.size() != index_max) {
    cpp11::stop("'index' must be a vector of length %d",
                index_max);
  }
  obj->reorder(index);
}

}
}
