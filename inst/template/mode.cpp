#include <mode/r/mode.hpp>
#include <dust/r/gpu_info.hpp>

{{model}}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_capabilities() {
  return mode::r::mode_capabilities<{{class}}>();
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_gpu_info() {
  return dust::gpu::r::gpu_info();
}

using {{name}}_{{target}} = mode::{{container}}<{{class}}>;

[[cpp11::register]]
SEXP mode_{{name}}_alloc(cpp11::list r_pars, bool pars_multi, cpp11::sexp r_time,
                         cpp11::sexp r_n_particles, int n_threads,
                         cpp11::sexp r_seed, bool deterministic,
                         cpp11::sexp r_gpu_config, cpp11::sexp r_ode_control) {
  return mode::r::mode_alloc<{{class}}>(r_pars, pars_multi, r_time, r_n_particles,
                                        n_threads, r_seed, deterministic,
                                        r_gpu_config, r_ode_control);
}

[[cpp11::register]]
SEXP mode_{{name}}_time(SEXP ptr) {
  return mode::r::mode_time<{{name}}_{{target}}>(ptr);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_rng_state(SEXP ptr, bool first_only, bool last_only) {
  return mode::r::mode_rng_state<{{name}}_{{target}}>(ptr, first_only, last_only);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_set_rng_state(SEXP ptr, cpp11::raws rng_state) {
  mode::r::mode_set_rng_state<{{name}}_{{target}}>(ptr, rng_state);
  return R_NilValue;
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_run(SEXP ptr, cpp11::sexp time_end) {
  return mode::r::mode_run<{{name}}_{{target}}>(ptr, time_end);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_simulate(SEXP ptr, cpp11::sexp time_end) {
  return mode::r::mode_simulate<{{name}}_{{target}}>(ptr, time_end);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_state(SEXP ptr, SEXP index) {
  return mode::r::mode_state<{{name}}_{{target}}>(ptr, index);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_ode_statistics(SEXP ptr) {
  return mode::r::mode_ode_statistics<{{name}}_{{target}}>(ptr);
}

[[cpp11::register]]
cpp11::sexp mode_{{name}}_update_state(SEXP ptr,
                                       SEXP pars,
                                       SEXP state,
                                       SEXP time,
                                       SEXP set_initial_state,
                                       SEXP index,
                                       SEXP reset_step_size) {
  return mode::r::mode_update_state<{{name}}_{{target}}>(ptr,
      pars, state, time, set_initial_state, index, reset_step_size);
}

[[cpp11::register]]
void mode_{{name}}_set_index(SEXP ptr, SEXP index) {
  return mode::r::mode_set_index<{{name}}_{{target}}>(ptr, index);
}

[[cpp11::register]]
void mode_{{name}}_set_stochastic_schedule(SEXP ptr, SEXP time) {
  return mode::r::mode_set_stochastic_schedule<{{name}}_{{target}}>(ptr, time);
}

[[cpp11::register]]
int mode_{{name}}_n_state(SEXP ptr) {
  return mode::r::mode_n_state<{{name}}_{{target}}>(ptr);
}

[[cpp11::register]]
void mode_{{name}}_reorder(SEXP ptr, SEXP index) {
  return mode::r::mode_reorder<{{name}}_{{target}}>(ptr, index);
}

[[cpp11::register]]
void mode_{{name}}_set_n_threads(SEXP ptr, int n_threads) {
  return mode::r::mode_set_n_threads<{{name}}_{{target}}>(ptr, n_threads);
}

[[cpp11::register]]
SEXP mode_{{name}}_resample(SEXP ptr, cpp11::doubles r_weights) {
  return mode::r::mode_resample<{{name}}_{{target}}>(ptr, r_weights);
  return R_NilValue;
}

[[cpp11::register]]
SEXP mode_{{name}}_set_data(SEXP ptr, cpp11::list data, bool shared) {
  mode::r::mode_set_data<{{name}}_{{target}}>(ptr, data, shared);
  return R_NilValue;
}

[[cpp11::register]]
SEXP mode_{{name}}_compare_data(SEXP ptr) {
  return mode::r::mode_compare_data<{{name}}_{{target}}>(ptr);
}

[[cpp11::register]]
SEXP mode_{{name}}_filter(SEXP ptr, SEXP time_end, bool save_trajectories,
                          cpp11::sexp time_snapshot,
                          cpp11::sexp min_log_likelihood) {
  return mode::r::mode_filter<{{name}}_{{target}}>(ptr, time_end,
                                                save_trajectories,
                                                time_snapshot,
                                                min_log_likelihood);
}
