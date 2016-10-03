doPer <- function(dat, from, to, period = 1){
    cuts <- seq(from, to, period)
    library(eha)
    dat$cal.per <- 0
    n <- length(cuts) - 1
    peri <- vector(mode = "list", length = n)
    for (i in 1:n){
        cat("i = ", i, " ")
        peri[[i]] <- cal.window(dat, c(cuts[i], cuts[i+1]))
        peri[[i]]$cal.per <- cuts[i]
    }
    do.call(rbind, peri)
}