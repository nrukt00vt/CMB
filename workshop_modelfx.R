############################################################
# workshop_modelfx.R
#
# Simple spatial SIR/SIRS model functions for workshop use.
#
# Core assumptions:
#   - Each county/patch has S, I, and R compartments.
#   - Transmission and recovery happen simultaneously within a timestep.
#   - Newly infected individuals do not recover until the next timestep.
#   - Initially infected individuals still contribute to transmission
#     during the same timestep in which some of them recover.
#   - Mobility is Eulerian: individuals physically move between patches.
#   - By default, S, I, and R all move using the same movement matrix.
############################################################


############################################################
# Initialize population object
############################################################

InitiatePop <- function(IDs,
                        initialInf_vec,
                        totalPop_vec,
                        beta_vec,
                        gamma_vec) {
  
  HPop <- list(
    ID = IDs,
    I = initialInf_vec,
    S = totalPop_vec - initialInf_vec,
    R = rep(0, length(initialInf_vec)),
    beta = beta_vec,
    gamma = gamma_vec,
    nTotal = totalPop_vec,
    frac_I = initialInf_vec / totalPop_vec
  )
  
  return(HPop)
}


############################################################
# Simultaneous epidemic timestep
#
# This replaces sequential exposure_timestep() followed by
# recovery_timestep().
#
# Important:
#   - Infections are calculated from old_S and old_I.
#   - Recoveries are calculated from old_I.
#   - Updates are applied only after all changes are calculated.
#
# This avoids timestep-ordering artifacts, such as:
#   - everyone recovering before they can transmit, or
#   - newly infected people recovering immediately.
############################################################

epidemic_timestep <- function(HPop,
                              waning_immunity = 0) {
  
  old_S <- HPop$S
  old_I <- HPop$I
  old_R <- HPop$R
  old_nTotal <- old_S + old_I + old_R
  
  # Avoid division by zero if a patch has zero population.
  old_nTotal_safe <- old_nTotal
  old_nTotal_safe[old_nTotal_safe == 0] <- NA
  
  # New infections generated during this timestep.
  newly_infected <- old_I * old_S * HPop$beta / old_nTotal_safe
  
  # Recoveries generated during this timestep.
  recovered_today <- old_I * HPop$gamma
  
  # Optional waning immunity: recovered individuals return to susceptible.
  # Set waning_immunity = 0 for a standard SIR model.
  losing_immunity_today <- old_R * waning_immunity
  
  # Replace NA values caused by zero-population patches with zero changes.
  newly_infected[is.na(newly_infected)] <- 0
  recovered_today[is.na(recovered_today)] <- 0
  losing_immunity_today[is.na(losing_immunity_today)] <- 0
  
  # Keep changes within available compartment sizes.
  newly_infected <- pmin(newly_infected, old_S)
  recovered_today <- pmin(recovered_today, old_I)
  losing_immunity_today <- pmin(losing_immunity_today, old_R)
  
  # Apply all changes simultaneously.
  HPop$S <- old_S - newly_infected + losing_immunity_today
  HPop$I <- old_I + newly_infected - recovered_today
  HPop$R <- old_R + recovered_today - losing_immunity_today
  
  # Numerical safety.
  HPop$S[HPop$S < 0] <- 0
  HPop$I[HPop$I < 0] <- 0
  HPop$R[HPop$R < 0] <- 0
  
  HPop$nTotal <- HPop$S + HPop$I + HPop$R
  HPop$frac_I <- ifelse(HPop$nTotal > 0, HPop$I / HPop$nTotal, 0)
  
  return(HPop)
}


############################################################
# Build movement matrix
#
# Expected mobility data columns:
#   - fr_pat: origin patch index
#   - to_pat: destination patch index
#   - fr_users: number of users in origin patch
#   - movers: number of observed movers from origin to destination
#
# movement_multiplier scales all off-diagonal movement rates.
#   - 0 = no movement
#   - 1 = baseline movement
#   - 2 = double movement
############################################################

build_movement_matrix <- function(HPop,
                                  mobmat,
                                  movement_multiplier = 1) {
  
  n_patches <- length(HPop$ID)
  movement_matrix <- matrix(0, n_patches, n_patches)
  
  daily_move <- subset(
    mobmat,
    !is.na(fr_pat) &
      !is.na(to_pat) &
      !is.na(fr_users) &
      !is.na(movers)
  )
  
  if (nrow(daily_move) > 0) {
    
    daily_move_mat <- daily_move[
      ,
      is.element(names(daily_move),
                 c("fr_pat", "to_pat", "fr_users", "movers"))
    ]
    
    daily_move_mat <- as.matrix(daily_move_mat)
    
    col_fr <- which(colnames(daily_move_mat) == "fr_pat")
    col_to <- which(colnames(daily_move_mat) == "to_pat")
    col_movers <- which(colnames(daily_move_mat) == "movers")
    col_users <- which(colnames(daily_move_mat) == "fr_users")
    
    origin_index <- daily_move_mat[, col_fr]
    destination_index <- daily_move_mat[, col_to]
    
    movement_rate <- daily_move_mat[, col_movers] /
      daily_move_mat[, col_users]
    
    movement_rate[is.na(movement_rate)] <- 0
    movement_rate[is.infinite(movement_rate)] <- 0
    
    movement_matrix[cbind(origin_index, destination_index)] <- movement_rate
  }
  
  # Original teaching model scaled mobility down by 0.1.
  # Retain that scale, but make it adjustable.
  movement_matrix <- movement_matrix * 0.1 * movement_multiplier
  
  # Make sure off-diagonal probabilities do not exceed 1.
  for (i in 1:n_patches) {
    off_diag_sum <- sum(movement_matrix[i, -i])
    
    if (off_diag_sum > 1) {
      movement_matrix[i, -i] <- movement_matrix[i, -i] / off_diag_sum
      off_diag_sum <- 1
    }
    
    movement_matrix[i, i] <- 1 - off_diag_sum
  }
  
  return(movement_matrix)
}


############################################################
# Move one compartment according to movement matrix
#
# This uses deterministic expected movement rather than stochastic
# binomial movement. That makes the workshop results reproducible
# and easier to interpret.
############################################################

move_compartment <- function(compartment_vec,
                             movement_matrix) {
  
  moved_matrix <- matrix(
    0,
    nrow = length(compartment_vec),
    ncol = length(compartment_vec)
  )
  
  for (i in 1:nrow(moved_matrix)) {
    moved_matrix[i, ] <- compartment_vec[i] * movement_matrix[i, ]
  }
  
  new_compartment_vec <- colSums(moved_matrix)
  
  return(new_compartment_vec)
}


############################################################
# Movement timestep
#
# By default, S, I, and R all move.
# This matches an Eulerian model where people physically move
# between patches while retaining disease status.
############################################################

movement_timestep <- function(HPop,
                              mobmat,
                              movement_multiplier = 1,
                              move_susceptible = TRUE,
                              move_infectious = TRUE,
                              move_recovered = TRUE) {
  
  movement_matrix <- build_movement_matrix(
    HPop = HPop,
    mobmat = mobmat,
    movement_multiplier = movement_multiplier
  )
  
  if (move_susceptible) {
    HPop$S <- move_compartment(HPop$S, movement_matrix)
  }
  
  if (move_infectious) {
    HPop$I <- move_compartment(HPop$I, movement_matrix)
  }
  
  if (move_recovered) {
    HPop$R <- move_compartment(HPop$R, movement_matrix)
  }
  
  HPop$nTotal <- HPop$S + HPop$I + HPop$R
  HPop$frac_I <- ifelse(HPop$nTotal > 0, HPop$I / HPop$nTotal, 0)
  
  return(HPop)
}


############################################################
# Master simulation function
############################################################

runSim <- function(HPop,
                   mobmat,
                   day_list,
                   movement_multiplier = 1,
                   waning_immunity = 0,
                   move_susceptible = TRUE,
                   move_infectious = TRUE,
                   move_recovered = TRUE,
                   verbose = TRUE) {
  
  epidemic_curve <- data.frame(
    day = c(),
    inf = c(),
    stringsAsFactors = FALSE
  )
  
  all_spread <- matrix(
    0,
    nrow = length(day_list),
    ncol = length(HPop$I)
  )
  
  colnames(all_spread) <- HPop$ID
  
  for (current_day in 1:length(day_list)) {
    
    if (verbose) {
      print(day_list[current_day])
    }
    
    # Transmission and recovery happen simultaneously here.
    HPop <- epidemic_timestep(
      HPop = HPop,
      waning_immunity = waning_immunity
    )
    
    # Movement occurs after the epidemic timestep.
    HPop <- movement_timestep(
      HPop = HPop,
      mobmat = mobmat,
      movement_multiplier = movement_multiplier,
      move_susceptible = move_susceptible,
      move_infectious = move_infectious,
      move_recovered = move_recovered
    )
    
    epidemic_curve <- rbind(
      epidemic_curve,
      data.frame(
        day = day_list[current_day],
        inf = sum(HPop$I)
      )
    )
    
    all_spread[current_day, ] <- HPop$I
  }
  
  all_spread_2 <- data.frame(day = day_list)
  all_spread_2 <- cbind(all_spread_2, all_spread)
  
  output <- list(
    HPop = HPop,
    epidemic_curve = epidemic_curve,
    all_spread = all_spread_2
  )
  
  return(output)
}