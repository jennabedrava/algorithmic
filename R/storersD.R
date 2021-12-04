#' Simulation of Storer's 3+3 D phase I design
#'
#' The storersD function simulates Storer's phase I D 3+3 design. The function provides
#' the proportion of times each dose was chosen as the Maximum Tolerated Dose (MTD).
# '
#' @param true_probs A vector of the true toxicity rates for each dose
#' @param npat Total number of patients available for the phase I study
#' @param nsim Number of simulations to run
#' @param seed If not empty, the seed to use for random generation
#' @return The proportion of simulated trials that identified each dose as the MTD
#'
#' @import stats
#'
#' @export
storersD <- function(true_probs, npat, nsim, seed){
  if (!is.null(seed)) {
    set.seed(seed)
  }
  storerD <- function(true_probs, npat) {
    if (npat %% 3 > 0) {
      npat = npat - (npat %% 3)
    }
    current_dose = 1
    next_dose = 1
    total_doses = length(true_probs)
    six = FALSE
    MTD = 0
    dlts = 0
    patients_treated = 0
    while (next_dose <= total_doses & patients_treated < npat) {
      current_dose = next_dose
      MTD = current_dose
      dlts = as.numeric(sum(rbinom(3, 1, true_probs[current_dose])))
      patients_treated = patients_treated + 3
      if (dlts == 0 & six == FALSE) { #no DLTs in first group of 3 or second group of 3 - either way, still icnreasing dose
        next_dose = current_dose + 1
        six = FALSE
        #print("no dlts")
      } else if (dlts == 1 & six == FALSE) { #1 DLT in the first group of 3
        next_dose = current_dose #stay on current dose
        six = TRUE
        MTD = current_dose - 1
        #print("1 dlts")
      } else if (dlts == 0 & six == TRUE) { #0 DLTs in the second group of 3
        next_dose = current_dose + 1 #increase dose
        six = FALSE
        #print("second round 1 dlt")
      } else { #at least 2 DLTs
        if (current_dose == 1){
          return("First dose too toxic")
          break
        }
        next_dose = current_dose - 1 #de-escalate
        MTD = current_dose - 1
      }
    }
    return(MTD)
  }

  sims = replicate(nsim, storerD(true_probs, npat))
  prop_at_each <- c()
  for(n in 1:length(true_probs)) {
    prop_at_each[n] = mean(sims == n)
  }
  return(list(prop_at_each = prop_at_each))
}
