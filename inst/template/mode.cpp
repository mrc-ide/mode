#include <mode/r/mode.hpp>

{{model}}

[[cpp11::register]]
SEXP mode_{{name}}_alloc(cpp11::list r_pars, double time) {
  return mode::r::mode_alloc<{{class}}>(r_pars, time);
}

[[cpp11::register]]
double mode_{{name}}_time(SEXP ptr) {
  return mode::r::mode_time<mode::container<{{class}}>>(ptr);
}
