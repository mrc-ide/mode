#include <mode/r/mode.hpp>

{{model}}

[[cpp11::register]]
SEXP mode_{{name}}_alloc(cpp11::list r_pars, double time,
                         size_t n_particles, size_t n_threads,
                         cpp11::sexp control, cpp11::sexp seed) {
  return mode::r::mode_alloc<{{class}}>(r_pars, time, n_particles, n_threads,
      control, seed);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_control(SEXP ptr) {
  return mode::r::mode_control<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
double mode_{{name}}_time(SEXP ptr) {
  return mode::r::mode_time<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_rng_state(SEXP ptr, bool first_only, bool last_only) {
  return mode::r::mode_rng_state<mode::container<{{class}}>>(ptr, first_only, last_only);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_set_rng_state(SEXP ptr, cpp11::raws rng_state) {
  mode::r::mode_set_rng_state<mode::container<{{class}}>>(ptr, rng_state);
  return R_NilValue;
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_run(SEXP ptr, double end_time) {
  return mode::r::mode_run<mode::container<{{class}}>>(ptr, end_time);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_simulate(SEXP ptr, cpp11::sexp end_time) {
  return mode::r::mode_simulate<mode::container<{{class}}>>(ptr, end_time);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_state_full(SEXP ptr) {
  return mode::r::mode_state_full<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_state(SEXP ptr, SEXP index) {
  return mode::r::mode_state<mode::container<{{class}}>>(ptr, index);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_stats(SEXP ptr) {
  return mode::r::mode_stats<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_update_state(SEXP ptr,
                                       SEXP pars,
                                       SEXP time,
                                       SEXP state,
                                       SEXP index,
                                       SEXP set_initial_state,
                                       SEXP reset_step_size) {
  return mode::r::mode_update_state<{{class}}>(ptr,
      pars, time, state, index, set_initial_state, reset_step_size);
}

[[cpp11::register]]
void mode_{{name}}_set_index(SEXP ptr, SEXP index) {
  return mode::r::mode_set_index<mode::container<{{class}}>>(ptr, index);
}

[[cpp11::register]]
void mode_{{name}}_set_stochastic_schedule(SEXP ptr, SEXP time) {
  return mode::r::mode_set_stochastic_schedule<mode::container<{{class}}>>(ptr, time);
}

[[cpp11::register]]
size_t mode_{{name}}_n_variables(SEXP ptr) {
  return mode::r::mode_n_variables<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
size_t mode_{{name}}_n_state_run(SEXP ptr) {
  return mode::r::mode_n_state_run<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
size_t mode_{{name}}_n_state_full(SEXP ptr) {
  return mode::r::mode_n_state_full<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
void mode_{{name}}_reorder(SEXP ptr, SEXP index) {
  return mode::r::mode_reorder<mode::container<{{class}}>>(ptr, index);
}

[[cpp11::register]]
void mode_{{name}}_set_n_threads(SEXP ptr, int n_threads) {
  return mode::r::mode_set_n_threads<mode::container<{{class}}>>(ptr, n_threads);
}

[[cpp11::register]]
bool mode_{{name}}_has_openmp() {
#ifdef _OPENMP
  return true;
#else
  return false;
#endif
}
