kohort <- function(cohort = "1821-1840", dat = kids){
    k1 <- dat[dat$cohort == cohort,]
    tfr <- numeric(3)
    lab <- levels(k1$hisclass)
    for (i in 1:3){
        d <- k1[k1$hisclass == lab[i], ]
        n.kids <- sum(d$event)
        mothers <- length(unique(d$id))
        exposure <- sum(d$exposure)
        tfr[i] <- n.kids / mothers
    }
    names(tfr) <- lab
    tfr
}