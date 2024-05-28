## Estimate regression models

# Shifts
ivs_shift <- c(shifts_race_matrices, shifts_sex_matrices, shifts_exp_matrices)

ols_shifts <- netlm(shift_matrix, ivs_shift, nullhyp = "classical")
ols_shifts$names <- c("(Intercept)", names(ivs_shift))

set.seed(5)
qap_shifts <- netlm(shift_matrix, ivs_shift, nullhyp = "qap", reps = 100)
qap_shifts$names <- c("(Intercept)", names(ivs_shift))

# Stops
ivs_stop <- c(stop_race_matrices, stop_sex_matrices, stop_exp_matrices)

ols_stop <- netlm(stop_matrix, ivs_stop, nullhyp = "classical")
ols_stop$names <- c("(Intercept)", names(ivs_stop))

set.seed(5)
qap_stop <- netlm(stop_matrix, ivs_stop, nullhyp = "qap", reps = 100)
qap_stop$names <- c("(Intercept)", names(ivs_stop))

# Arrests
ivs_arrest <- c(arrest_race_matrices, arrest_sex_matrices, arrest_exp_matrices)

ols_arrest <- netlm(arrest_matrix, ivs_arrest, nullhyp = "classical")
ols_arrest$names <- c("(Intercept)", names(ivs_arrest))

set.seed(5)
qap_arrest <- netlm(arrest_matrix, ivs_arrest, nullhyp = "qap", reps = 100)
qap_arrest$names <- c("(Intercept)", names(ivs_arrest))
