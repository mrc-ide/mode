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
cpp11::sexp mode_{{name}}_solve(SEXP ptr, int end_time) {
  return mode::r::mode_solve<mode::container<{{class}}>>(ptr, end_time);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_stats(SEXP ptr) {
  return mode::r::mode_stats<mode::container<{{class}}>>(ptr);
}

[[cpp11::register]]
void mode_{{name}}_update_state(SEXP ptr, SEXP time) {
  return mode::r::mode_update_state<mode::container<{{class}}>>(ptr, time);
}
