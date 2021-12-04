#' Simulation of Storer's 3+3 phase I design
#'
#' The storersA function simulates Storer's phase I 3+3 design. The function provides
#' the proportion of times each dose was chosen as the Maximum Tolerated Dose (MTD).
# '
#' @param true_probs A vector of the true toxicity rates for each dose
#' @param nsim Number of simulations to run
#' @param seed If not empty, the seed to use for random generation
#' @return The proportion of simulated trials that identified each dose as the MTD
#'
#' @import stats
#'
#' @export
storersA <- function(true_probs, nsim, seed) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  storerA <- function(true_probs) {
    total_doses = length(true_probs)
    current_dose = 1
    next_dose = 1
    six = FALSE
    MTD = 0
    dlts = 0
    while (next_dose <= total_doses) {
      current_dose = next_dose
      MTD = current_dose
      dlts = as.numeric(sum(rbinom(3, 1, true_probs[current_dose])))
      if (dlts == 0 & six == FALSE) { #no DLTs in first group of 3 or second group of 3 - either way, still icnreasing dose
        next_dose = current_dose + 1
        six = FALSE
        #print("no dlts")
      } else if (dlts == 1 & six == FALSE) { #1 DLT in the first group of 3
        next_dose = current_dose #stay on current dose
        six = TRUE
        #print("1 dlts")
      } else if (dlts == 0 & six == TRUE) { #0 DLTs in the second group of 3
        next_dose = current_dose + 1 #increase dose
        six = FALSE
        #print("second round 1 dlt")
      } else { #at least 2 DLTs
        if (current_dose == 1){
          return("First dose too toxic")
        }
        MTD = current_dose - 1
        #print("2 or more dlts BREAK")
        break
      }
    }
    return(MTD)
  }

  sims = replicate(nsim, storerA(true_probs))
  prop_at_each <- c()
  for(n in 1:length(true_probs)) {
    prop_at_each[n] = mean(sims == n)
  }
  return(list(prop_at_each = prop_at_each))
}

