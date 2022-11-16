#pragma once


#include <cpp11/external_pointer.hpp>
#include <cpp11/strings.hpp> // required to avoid link error only
#include <cpp11/list.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/protect.hpp>

#include <dust/random/random.hpp>
#include <dust/r/random.hpp>
#include <dust/utils.hpp>

#include <mode/mode.hpp>
#include <mode/r/helpers.hpp>

namespace mode {
namespace r {

template <typename T>
cpp11::list mode_alloc(cpp11::list r_pars, double time, size_t n_particles,
                       size_t n_threads,
                       cpp11::sexp control, cpp11::sexp r_seed) {
  auto pars = mode::mode_pars<T>(r_pars);
  auto seed = dust::random::r::as_rng_seed<typename T::rng_state_type>(r_seed);
  auto ctl = mode::r::validate_control(control);
  cpp11::sexp info = mode_info(pars);
  mode::r::validate_positive(n_threads, "n_threads");
  mode::r::validate_positive(n_particles, "n_particles");
  container<T> *d = new mode::container<T>(pars, time, n_particles,
                                           n_threads, ctl, seed);
  cpp11::external_pointer<container<T>> ptr(d, true, false);
  return cpp11::writable::list({ptr, info});
}

template <typename T>
void mode_set_n_threads(SEXP ptr, size_t n_threads) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  mode::r::validate_positive(n_threads, "n_threads");
  obj->set_n_threads(n_threads);
}

template <typename T>
cpp11::sexp mode_control(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  auto ctl = obj->ctl();
  return mode::r::control(ctl);
}

template <typename T>
double mode_time(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->time();
}

template <typename T>
cpp11::sexp mode_rng_state(SEXP ptr, bool first_only, bool last_only) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  using rng_state_type = typename T::rng_state_type;
  const auto state = obj->rng_state();
  if (first_only && last_only) {
    cpp11::stop("Only one of 'first_only' or 'last_only' may be TRUE");
  }
  if (last_only) {
    // This is used in dust when we have n + 1 rng streams, with the
    // last one being the particle filter stream - we don't have that
    // set up here yet, so just error instead. See dust for
    // implementation details, and mrc-3360
    cpp11::stop("'last_only' not supported for mode models");
  }
  const size_t n = first_only ? rng_state_type::size() : state.size();
  const size_t len = sizeof(typename rng_state_type::int_type) * n;
  cpp11::writable::raws ret(static_cast<R_xlen_t>(len));
  std::memcpy(RAW(ret), state.data(), len);
  return ret;
}

template <typename T>
void mode_set_rng_state(SEXP ptr, cpp11::raws rng_state) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  using int_type = typename T::rng_state_type::int_type;
  auto prev_state = obj->rng_state();
  size_t len = prev_state.size() * sizeof(int_type);
  if ((size_t)rng_state.size() != len) {
    cpp11::stop("'rng_state' must be a raw vector of length %d (but was %d)",
                len, rng_state.size());
  }
  std::vector<int_type> state(prev_state.size());
  std::memcpy(state.data(), RAW(rng_state), len);
  obj->set_rng_state(state);
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
void mode_set_stochastic_schedule(SEXP ptr, cpp11::sexp r_time) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();

  std::vector<double> time;
  if (r_time != R_NilValue) {
    time = cpp11::as_cpp<std::vector<double>>(cpp11::as_doubles(r_time));
    for (size_t i = 1; i < time.size(); ++i) {
      if (time[i] <= time[i - 1]) {
        cpp11::stop("schedule must be strictly increasing; see time[%d]",
                    i + 1);
      }
    }
  }
  obj->set_stochastic_schedule(time);
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

  std::vector<double> dat(obj->n_state_run() * obj->n_particles());
  obj->state_run(dat);
  return mode::r::state_array(dat, obj->n_state_run(), obj->n_particles());
}

template <typename T>
cpp11::sexp mode_simulate(SEXP ptr, cpp11::sexp r_end_time) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  obj->check_errors();
  const auto end_time = as_vector_double(r_end_time, "end_time");
  const auto n_time = end_time.size();
  if (n_time == 0) {
    cpp11::stop("'end_time' must have at least one element");
  }
  if (end_time[0] < obj->time()) {
    cpp11::stop("'end_time[1]' must be at least %f", obj->time());
  }
  for (size_t i = 1; i < n_time; ++i) {
    if (end_time[i] < end_time[i - 1]) {
      cpp11::stop("'end_time' must be non-decreasing (error on element %d)",
                  i + 1);
    }
  }

  auto dat = obj->simulate(end_time);

  return mode::r::state_array(dat, obj->n_state_run(), obj->n_particles(),
                              n_time);
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
  const auto n_particles = obj->n_particles();
  std::vector<size_t> dat(3 * n_particles);
  obj->statistics(dat);
  auto ret = mode::r::stats_array(dat, n_particles);
  auto step_times = obj->debug_step_times();
  if (obj->ctl().debug_record_step_times) {
    auto step_times = obj->debug_step_times();
    cpp11::writable::list r_step_times(step_times.size());
    for (size_t i = 0; i < n_particles; ++i) {
      r_step_times[i] = cpp11::as_sexp(step_times[i]);
    }
    ret.attr("step_times") = r_step_times;
  }
  return ret;
}

template <typename T>
cpp11::sexp mode_update_state(SEXP ptr, SEXP r_pars, SEXP r_state, SEXP r_time,
                              SEXP r_set_initial_state,
                              SEXP r_index, SEXP r_reset_step_size) {
  mode::container<T> *obj =
      cpp11::as_cpp < cpp11::external_pointer<mode::container<T>>>(ptr).get();

  std::vector<size_t> index;
  const size_t index_max = obj->n_variables();
  if (r_index != R_NilValue) {
    index = mode::r::r_index_to_index(r_index, index_max);
  } else {
    index.clear();
    index.reserve(index_max);
    for (size_t i = 0; i < index_max; ++i) {
      index.push_back(i);
    }
  }

  const size_t n_state_full = obj->n_state_full();

  auto set_initial_state = mode::r::validate_set_initial_state(r_state,
                                                               r_pars,
                                                               r_time,
                                                               r_set_initial_state);
  auto reset_step_size = mode::r::validate_reset_step_size(r_time,
                                                           r_pars,
                                                           r_reset_step_size);
  auto time = mode::r::validate_time(r_time);
  auto state = mode::r::validate_state(r_state,
                                       index.size(),
                                       n_state_full,
                                       static_cast<int>(obj->n_particles()));
  cpp11::sexp ret = R_NilValue;
  if (r_pars != R_NilValue) {
    auto pars = mode::mode_pars<T>(r_pars);
    obj->set_pars(pars);
    ret = mode_info<T>(pars);
  }
  // NOTE: there's no equivalent to this in dust, with all the work
  // done at the 'r/' level, so we don't try and preserve much about
  // the order of variables here (partly because everywhere else
  // within the ode work we do (time, state) not (state, time) as on
  // entry here.
  obj->update_state(time, state, index, set_initial_state, reset_step_size);
  return ret;
}

template <typename T>
size_t mode_n_state_full(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->n_state_full();
}

template <typename T>
size_t mode_n_state_run(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->n_state_run();
}

template <typename T>
size_t mode_n_variables(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->n_variables();
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
