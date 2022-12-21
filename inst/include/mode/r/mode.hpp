#pragma once


#include <cpp11/external_pointer.hpp>
#include <cpp11/strings.hpp> // required to avoid link error only
#include <cpp11/list.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/protect.hpp>

#include <dust/random/random.hpp>
#include <dust/r/helpers.hpp>
#include <dust/r/random.hpp>
#include <dust/r/utils.hpp>
#include <dust/types.hpp>
#include <dust/utils.hpp>

#include <mode/mode.hpp>
#include <mode/r/helpers.hpp>

namespace mode {
namespace r {

template <typename T>
cpp11::list mode_alloc(cpp11::list r_pars, bool pars_multi, cpp11::sexp r_time,
                       cpp11::sexp r_n_particles, size_t n_threads,
                       cpp11::sexp r_seed, bool deterministic,
                       cpp11::sexp r_gpu_config, cpp11::sexp r_ode_control) {
  if (deterministic) {
    cpp11::stop("Deterministic mode not supported for mode models");
  }
  auto pars = dust::dust_pars<T>(r_pars);
  auto seed = dust::random::r::as_rng_seed<typename T::rng_state_type>(r_seed);
  auto ctl = mode::r::validate_ode_control(r_ode_control);
  const double t0 = 0;
  const auto time = dust::r::validate_time<double>(r_time, t0, "time");
  cpp11::sexp info = dust::dust_info(pars);
  mode::r::validate_positive(n_threads, "n_threads");
  auto n_particles = cpp11::as_cpp<int>(r_n_particles);
  mode::r::validate_positive(n_particles, "n_particles");
  dust_ode<T> *d = new mode::dust_ode<T>(pars, time, n_particles,
                                           n_threads, ctl, seed);
  cpp11::external_pointer<dust_ode<T>> ptr(d, true, false);
  cpp11::writable::integers r_shape =
    dust::r::vector_size_to_int(ptr->shape());
  auto r_ctl = mode::r::control(ctl);
  return cpp11::writable::list({ptr, info, r_shape, r_gpu_config, r_ctl});
}

template <typename T>
void mode_set_n_threads(SEXP ptr, size_t n_threads) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  mode::r::validate_positive(n_threads, "n_threads");
  obj->set_n_threads(n_threads);
}

template <typename T>
SEXP mode_time(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return cpp11::as_sexp(obj->time());
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
cpp11::sexp mode_run(SEXP ptr, cpp11::sexp r_time_end) {
  using time_type = typename T::time_type;
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  const auto time_end =
    dust::r::validate_time<time_type>(r_time_end, obj->time(), "time_end");
  obj->run(time_end);

  std::vector<double> dat(obj->n_state_run() * obj->n_particles());
  obj->state(dat);
  return mode::r::state_array(dat, obj->n_state_run(), obj->n_particles());
}

template <typename T>
cpp11::sexp mode_simulate(SEXP ptr, cpp11::sexp r_time_end) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  using time_type = typename T::time_type;
  const auto time_end =
    dust::r::validate_time<std::vector<time_type>>(r_time_end, obj->time(), "time_end");
  obj->check_errors();
  auto dat = obj->simulate(time_end);

  return mode::r::state_array(dat, obj->n_state_run(), obj->n_particles(),
                              time_end.size());
}

template <typename T>
cpp11::sexp mode_state_full(T *obj) {
  std::vector<double> dat(obj->n_state_full() * obj->n_particles());
  obj->state_full(dat);
  return mode::r::state_array(dat, obj->n_state_full(), obj->n_particles());
}

template <typename T>
cpp11::sexp mode_state_select(T *obj, SEXP r_index) {
  const size_t index_max = obj->n_state_full();
  const std::vector <size_t> index =
      mode::r::r_index_to_index(r_index, index_max);
  size_t n = index.size();
  std::vector<double> dat(n * obj->n_particles());
  obj->state(index, dat);
  return mode::r::state_array(dat, n, obj->n_particles());
}

template <typename T>
cpp11::sexp mode_state(SEXP ptr, SEXP r_index) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  if (r_index == R_NilValue) {
    return mode_state_full(obj);
  } else {
    return mode_state_select(obj, r_index);
  }
}

template <typename T>
cpp11::sexp mode_ode_statistics(SEXP ptr) {
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
  mode::dust_ode<T> *obj =
      cpp11::as_cpp < cpp11::external_pointer<mode::dust_ode<T>>>(ptr).get();

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
    auto pars = dust::dust_pars<T>(r_pars);
    obj->set_pars(pars);
    ret = dust::dust_info<T>(pars);
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
size_t mode_n_state(SEXP ptr) {
  T *obj = cpp11::as_cpp<cpp11::external_pointer<T>>(ptr).get();
  return obj->n_state();
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

template <typename T>
cpp11::sexp mode_capabilities() {
  using namespace cpp11::literals;
#ifdef _OPENMP
  bool openmp = true;
#else
  bool openmp = false;
#endif
  bool gpu = false;
  bool compare = false;
  using real_type = double; // typename T::real_type;
  auto real_size = sizeof(real_type);
  auto rng_algorithm =
    dust::random::r::algorithm_name<typename T::rng_state_type>();
  return cpp11::writable::list({"openmp"_nm = openmp,
                                "compare"_nm = compare,
                                "gpu"_nm = gpu,
                                "rng_algorithm"_nm = rng_algorithm,
                                "real_size"_nm = real_size * CHAR_BIT});
}

template <typename T>
SEXP mode_resample(SEXP ptr, cpp11::doubles r_weights) {
  cpp11::stop("Can't use resample with mode models");
  return R_NilValue; // unreachable
}

// This is the approach used in dust, but though it switches using the
// value of the data type, which we don't support yet.
inline void disable_method(const char * name) {
  cpp11::stop("The '%s' method is not supported for this class", name);
}

template <typename T>
void mode_set_data(SEXP ptr, cpp11::list r_data, bool data_is_shared) {
  disable_method("set_data");
}

template <typename T>
cpp11::sexp mode_compare_data(SEXP ptr) {
  disable_method("compare_data");
  return R_NilValue; // #nocov never gets here
}

template <typename T>
cpp11::sexp mode_filter(SEXP ptr, SEXP time_end, bool save_trajectories,
                        cpp11::sexp time_snapshot,
                        cpp11::sexp min_log_likelihood) {
  disable_method("filter");
  return R_NilValue; // #nocov never gets here
}

}
}
