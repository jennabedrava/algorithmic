#' Simulation of Biased Coin phase I design
#'
#' The biasedcoin function simulates a biased coin phase I design. The function provides
#' the proportion of times each dose was chosen as the Maximum Tolerated Dose (MTD) and
#' the average Dose Limiting Toxicity (DLT) rate.
# '
#' @param true_probs A vector of the true toxicity rates for each dose
#' @param start Starting dose
#' @param q_esc The biased coin's probability of "success"
#' @param npat Total number of patients available for the phase I study
#' @param nsim Number of simulations to run
#' @param seed If not empty, the seed to use for random generation
#' @return The proportion of simulated trials that identified each dose as the MTD
#'
#' @import stats
#'
#' @export
biasedcoin <- function(true_probs, start, q_esc, npat, nsim, seed) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  decision <- matrix(NA, nrow=nsim, ncol=npat)
  dose_assignment = matrix(NA, nrow=nsim, ncol=npat)
  dose_assignment[,1] <- start #The first patient in each simulation starts at starting dose
  dlt <- matrix(NA, nrow=nsim, ncol=npat)
  dlt[,1] <- rbinom(nsim, 1, true_probs[dose_assignment[,1]])
  decision[,1] <- 0 #no decision needs to be made before first round - everyone gets starting dose
  for(k in 2:npat)
  {
    #DECISION MATRIX
    for (i in 1:nsim) {
      if (dlt[i, k-1] ==  0) {
        decision[i,k] = rbinom(1, 1, q_esc)
      } else {
        decision[i,k] = -1
      }
    }
    #ADDING K COLUMN TO DOSE ASSIGNMENT MATRIX
    for (j in 1:nsim) {
      if (dose_assignment[j,k-1] == start) {
        dose_assignment[j,k] = pmax(start, decision[j,k] + dose_assignment[j,(k-1)])
        #because we can't de-escalate below starting dose level
      } else if (dose_assignment[j,k-1] == length(true_probs) & decision[j,k] == 1) {
        dose_assignment[j,k] = pmin(length(true_probs), decision[j,k] + dose_assignment[j,(k-1)])
        #because we can't escalate over last dose level
      } else {
        dose_assignment[j,k] = decision[j,k] + dose_assignment[j,(k-1)]
        #case where there's no boundary limitations
      }
    }
    #ADDING K COLUMN TO DLT MATRIX
    for(l in 1:nsim){
      for(m in 1:length(true_probs)){
        if(dose_assignment[l,k]==m){
          dlt[l,k]= rbinom(1, 1, true_probs[m])
        }
      }
    }
  }
  prop_at_each <- c()
  for(n in 1:length(true_probs)) {
    prop_at_each[n] = mean(dose_assignment==n)
  }
  avg_dlt_rate = mean(dlt==1)
  return(list(prop_at_each = prop_at_each, avg_dlt_rate = avg_dlt_rate))
}
