tabu <- function(surv.lim = 1){
    ## tablate number of births by socpo and decennium
    library(skum)
    load("data/kids.rda")
    births <- kids[ ,
                      c("id", "ch.id", "birthdate", "enter", "exit", "socpo",
                        "farmer", "marAge", "marEnd")]
    chinfo <- per[per$id %in% births$ch_id, c("id", "foddat", "doddat")]
    chinfo$death.age <- as.numeric(chinfo$doddat - chinfo$foddat) / 365.2425
    chinfo$death.age[is.na(chinfo$death.age)] <- 25
    indx <- match(births$ch_id, chinfo$id)
    births$death.age <- chinfo$death.age[indx]
    ##return(births)
    births <- births[!is.na(births$socpo), ]
    births$marEnd <- pmin(births$marEnd, 50) # "end of fertility period..."
    births$date <- births$birthdate + births$exit
    births <- births[order(births$id, births$date), ]
    births <- births[births$marEnd >= births$exit, ]
    births <- rc(births)
    ab <- with(births, tapply(id, id, length))
    ad <- with(births, tapply(death.age <= surv.lim, id, sum))
    
    ub <- births[births$lopnr == 1, ]
    ##cat("length(ab) = ", length(ab), "\n")
    ##cat("NROW(ub) = ", NROW(ub), "\n")
    indx <- match(ub$id, names(ab)) # Not necessary, but ...
    ub$n.births <- ab[indx]
    indx <- match(ub$id, names(ad))
    ub$n.deaths <- ad[indx]
    ub$exposure <- ub$marEnd - ub$marAge
    ub$cohort <- cut(ub$birthdate, seq(1821, 1931, by = 10),
                     labels = c("1821-30", "1831-40", "1841-50", "1851-60",
                                "1861-70", "1871-80", "1881-90", "1891-1900",
                                "1901-10", "1911-20", "1921-30"))
    ub
}
    
