cohRates <- function(deaths = FALSE, surv.lim = 1, rate = TRUE){
    source("R/tabu.R")
    ub <- tabu(surv.lim = surv.lim)
    if (deaths) ub$n.births <- ub$n.births - ub$n.deaths
    if (!rate) ub$exposure <- 1 # Count number of births
    fil <- aggregate(ub[, c("n.births", "exposure")],
                     by = ub[, c("cohort", "socpo")],
                     FUN = sum)

    ##out <- matrix(0, nrow = length(unique(fil$cohort)),
      ##            ncol = length(unique(fil$socpo)))
    
    fil$rate = fil$n.births / fil$exposure
    out <- matrix(fil$rate,
                  ncol = length(unique(fil$cohort)),
                  byrow = TRUE)
    out
}
