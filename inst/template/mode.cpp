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

[[cpp11::register]]
std::vector<double> mode_{{name}}_solve(SEXP ptr, int end_time) {
return mode::r::mode_solve<mode::container<{{class}}>>(ptr, end_time);
}
