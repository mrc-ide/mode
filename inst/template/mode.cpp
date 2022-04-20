#include <mode/r/mode.hpp>

{{model}}

[[cpp11::register]]
SEXP mode_{{name}}_alloc(cpp11::list r_pars, double time, size_t n_particles) {
  return mode::r::mode_alloc<{{class}}>(r_pars, time, n_particles);
}

[[cpp11::register]]
double mode_{{name}}_time(SEXP ptr) {
  return mode::r::mode_time<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_run(SEXP ptr, int end_time) {
  return mode::r::mode_run<mode::container<{{class}}>>(ptr, end_time);
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
void mode_{{name}}_update_state(SEXP ptr,
                                SEXP state,
                                SEXP time,
                                SEXP pars,
                                SEXP set_initial_state,
                                SEXP reset_step_size) {
  return mode::r::mode_update_state<{{class}}>(ptr,
      state, time, pars, set_initial_state, reset_step_size);
}

[[cpp11::register]]
void mode_{{name}}_set_index(SEXP ptr, SEXP index) {
  return mode::r::mode_set_index<mode::container<{{class}}>>(ptr, index);
}

[[cpp11::register]]
void mode_{{name}}_reorder(SEXP ptr, SEXP index) {
  return mode::r::mode_reorder<mode::container<{{class}}>>(ptr, index);
}
