fertil3 <- function(){
    ## A complete rewrite and a new approach to find births within marriage,
    ## and the mothers. Now we get births from the obs file via 'starttyp == 2'
    ## and .
    ## Then the mothers are 
    ## Must be run from '..'! (see save at bottom)
    
    library(skum) # loads 'eha' as well...
    
    infants <- obs[obs$starttyp == 2 &
                    obs$startdat > as.Date("1820-12-31"), ]
    ## Remove some uninteresting stuff from 'infants':
    infants$civst <- infants$startdatmin <- infants$startdatmax <- infants$pid <- NULL
    infants$startdat_ind <- infants$slutdatmin <- infants$slutdatmax <- NULL
    infants$slutdat_ind <- infants$narvaro_gen <- infants$pers_lopnr <- NULL
    infants$validIn <- infants$validOut <- infants$valid <- infants$lopnr <- NULL
    infants$antrec <- infants$starttyp <- NULL
    
    ## Put on info from 'per': 
    
    indx <- match(infants$id, per$id)
    infants$m.id <- per$mid[indx]
    infants$ab <- per$ab[indx]
    infants$df <- per$df[indx]
    infants$fb <- per$fb[indx]
    infants$parity <- per$paritet_g[indx]
    is.na(infants$parity) <- infants$parity %in% c(-1, 99)
    
    infants <- infants[infants$ab == 1, ]  ## remove everything but 'äkta'!
    infants <- infants[!is.na(infants$m.id), ]  ## remove unknown mother's id
    
    ## Put on 'mother stuff':
    indx <- match(infants$m.id, per$id)
    infants$m.foddat <- per$foddat[indx]
    infants$marStart  <- per$marStart[indx]
    infants$marEnd <- per$marEnd[indx]
    infants <- infants[!is.na(infants$m.foddat), ] # Must know m.foddat!
    
    infants <- infants[infants$m.foddat > as.Date("1800-12-31") &
                           infants$m.foddat < as.Date("1931-01-01"), ]
    
    moth <- obs[obs$id %in% infants$m.id & obs$civst == "married", ]
    ## Nw, it happens that some mothers in 'infants' do not exist in 
    ## 'obs': we remove them.
    infants <- infants[infants$m.id %in% moth$id, ]
    
    from <- with(moth, tapply(enter, id, min))
    
    indx <- match(moth$id, names(from))
    moth$from <- from[indx]
    to <- with(moth, tapply(exit, id, max))
    indx <- match(moth$id, names(to), max)
    moth$to <- to[indx]
    
    indx <- match(infants$m.id, moth$id)
    
    infants$from <- moth$from[indx]
    infants$to <- moth$to[indx]
    
    ##return(moth)
    
    ##return(infants) # Looks OK  here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    
    ## Now, we turn the 'infants' file into a 'mothers' file by 
    ## id --> ch.id, m.id --> id and sort by mother:
    
    infants$ch.id <- infants$id
    infants$id <- infants$m.id
    infants$m.id <- NULL
    infants$m.birthdate <- as.numeric(toTime(infants$m.foddat))
    infants$exit <- infants$birthdate - infants$m.birthdate
    infants$ch.birthdate <- infants$birthdate
    infants$birthdate <- infants$m.birthdate
    infants$m.birthdate <- NULL
    ## Remove some nuisance:
    infants$startdat <- infants$slutdat <- NULL
    infants$enter <- infants$ab <- infants$df <- infants$m.foddat <- NULL
    infants$sluttyp <- NULL
    
    ## Remove births ('exit') before 20 or after 50:
    
    infants <- infants[infants$exit >= 20 & infants$exit < 50, ]
    
    infants <- infants[order(infants$id, infants$ch.birthdate), ]
    
    infants <- rc(infants)
    
    ##return(infants) # inf55.2
    
    ## Now, adjust 'from' and 'to'
    max.exit <- with(infants, tapply(exit, id, max))
    min.exit <- with(infants, tapply(exit, id, min))
    indx <- match(infants$id, names(min.exit))
    infants$min.exit <- min.exit[indx]
    indx <- match(infants$id, names(max.exit))
    infants$max.exit <- max.exit[indx]
    
    infants$from <- pmax(infants$from, 20)
    infants$from <- pmin(infants$from, infants$min.exit)
    
    infants$to <- pmin(infants$to, 50)
    infants$to <- pmax(infants$to, infants$max.exit)
    
    ## Now, put on 'enter': 'exit on previous record, if same mother:
    infants$enter <- 0
    infants$enter[-1] <- infants$exit[-NROW(infants)]
    infants$enter[infants$lopnr == 1] <- infants$from[infants$lopnr == 1]
    infants$event <- TRUE
    
    ## Put on 'last' record by copying present last record
    
    ##return(infants)
    infants <- rc(infants)
    last <- infants[infants$lopnr == infants$antrec, ]
    last$enter <- last$exit
    last$exit <- last$to
    last$parity <- last$parity + 1
    last$event <- FALSE
    last$ch.id <- NA
    last$ch.birthdate <- NA
    last$sex <- NA
    
    infants <- rbind(infants, last)
    infants$parity <- infants$parity - 1
    infants <- infants[order(infants$id, infants$enter), ]
    infants <- rc(infants)
    rownames(infants) <- 1:NROW(infants)

    ## Clean:
    infants$foddat <- infants$max.exit <- infants$min.exit <- NULL   

    ##return(infants) # inf55.1: Wrong here!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## Now the childless married must be put on board!
    
    chless <- per$id[!is.na(per$foddat) &
                           per$foddat > as.Date("1800-12-31") &
                           per$foddat < as.Date("1931-01-01") &
                           !is.na(per$kon) &
                           per$kon == "female" &
                         !is.na(per$marStart) &
                         !is.na(per$marEnd) &
                           !(per$id %in% infants$id)]
    
    chless <- unique(obs$id[obs$id %in% chless & obs$civst == "married"])
    ##cat("No. of childless is ", length(chless), "\n")
    
    nomothers <- obs[obs$id %in% chless & obs$civst == "married", ]
    
    nomothers <- nomothers[nomothers$exit > 20 & nomothers$enter < 50, ]
    
    nomothers$exit <- pmin(nomothers$exit, 50)
    nomothers$enter <- pmax(nomothers$enter, 20)
    
    ##return(nomothers)
    kids2 <- data.frame(id = infants$id,
                        birthdate = infants$birthdate,
                        ch.id = infants$ch.id,
                        sex = infants$sex,
                        parity = infants$parity,
                        fb = infants$fb,
                        ch.birthdate = infants$ch.birthdate,
                        ch.deathdate = NA,
                        region = infants$region,
                        nofrs = infants$nofrs,
                        ort = infants$ort,
                        ortnmn = infants$ortnmn,
                        urban = infants$ortnmn %in% c("UMEÅ", "SKELLEFTEÅ STAD"),
                        enter = infants$enter,
                        exit = infants$exit,
                        event = infants$event,
                        socst = infants$socst,
                        hisclass = infants$hisclass,
                        from = infants$from,
                        to = infants$to)
                        
                        
    nokids <- data.frame(id = nomothers$id,
                         birthdate = nomothers$birthdate,
                         ch.id = NA,
                         sex = NA,
                         parity = 0,
                         fb = NA,
                         ch.birthdate = NA,
                         ch.deathdate = NA,
                         region = nomothers$region,
                         nofrs = nomothers$nofrs,
                         ort = nomothers$ort,
                         ortnmn = nomothers$ortnmn,
                         urban = nomothers$ortnmn %in% c("UMEÅ", "SKELLEFTEÅ STAD"),
                         enter = nomothers$enter,
                         exit = nomothers$exit,
                         event = FALSE,
                         socst = nomothers$socst,
                         hisclass = nomothers$hisclass,
                         from = nomothers$enter,
                         to = nomothers$exit)

    kids2 <- rbind(nokids, kids2)

    kids2 <- kids2[order(kids2$id, kids2$ch.birthdate), ]
    
    kids2 <- rc(kids2)
    indx <- match(kids2$ch.id, per$id)
    kids2$ch.deathdate <- as.numeric(toTime(per$doddat[indx]))
    aj <- kids2$enter >= kids2$exit
    kids2$enter[aj] <- kids2$exit[aj] - 0.001 # To calm coxph and coxreg!
    kids2$inf.death <- with(kids2, !is.na(ch.birthdate) & !is.na(ch.deathdate) &
                                as.numeric(toTime(ch.deathdate)) - 
                                as.numeric(toTime(ch.birthdate)) < 1)
    rownames(kids2) <- 1:NROW(kids2)
    
    kids2$hisclass <- unlist(with(kids2, tapply(hisclass, id, pushForward)))
    kids2$socst <- unlist(with(kids2, tapply(socst, id, pushForward)))
    ##kids2$socBranch <- unlist(with(kids2, tapply(socBranch, id, pushForward)))
    kids2$parish <- factor(kids2$nofrs, labels = c("Umeå.land", "Umeå", 
                                                   "Skellefteå.land", "Skellefteå",
                                                   "Byske", "Bureå", "Yttersfors",
                                                   "Norsjö", "Jörn", "Malå"))
    levels(kids2$parish)[7] <- "Byske" # Yttersfors --> Byske
    kids2 <- kids2[!is.na(kids2$hisclass), ]
    
    kids <- kids2
    save(kids, file = "data/kids.rda")   
    cat("Saved 'kids' in 'data/kids.rda'!!\n")
    return(dim(kids))                    
    ##return(nomothers)
    ##return(infants)
    
########################### This is the end !!!!!!! ##########################
    
    ##This is old stuff:
    married <- skum::obs[with(skum::obs, birthdate > 1800.99 &        ## NOTE
                            birthdate < 1935 & sex == "female" &
                                civst == "married"), ]
    married <- married[order(married$id, married$enter), ]
    married <- married[married$enter < 50, ]
    married$exit <- pmin(married$exit, 50)
    married$urban <- married$ortnmn %in% c("UMEÅ", "SKELLEFTEÅ STAD")
    married <- married[, c("id", "region", "socst", "starttyp", "pid",
                           "sluttyp", "birthdate", "enter", "exit", "socpo",
                           "hisclass", "socBranch", "socStatus", "urban")]
    ## We want them to be present sometime before age 20:
    earlyBirds <- unique(skum::obs$id[skum::obs$enter < 20])
    married <- married[married$id %in% earlyBirds, ]
    married <- rc(married)

    kids <- skum::per[skum::per$mid %in% married$id,
                      c("id", "mid", "region", "foddat", "doddat", "ab", "fodhfrs")]
    kids$urban <- kids$fodhfrs %in% c(82780, 82981) # Added 10 oct 2016
    kids$fodhfrs <- NULL
    kids <- kids[kids$ab %in% c(0, 1, 3), ]
    kids <- kids[!is.na(kids$foddat), ]       ## New in '3'
    ##return(kids)
    kids$birthdate <- as.numeric(toTime(kids$foddat))
    kids$deathdate <- as.numeric(toTime(kids$doddat))
    kids$foddat <- kids$doddat <- NULL
    marStart <- with(married, tapply(enter, id, min))
    marEnd <- with(married, tapply(exit, id, max))
    indx <- with(married, tapply(id, id))
    married$marStart <- marStart[indx]
    married$marEnd <- marEnd[indx]
    
    mothers <- married[married$id %in% kids$mid, ]
    nomothers <- married[!(married$id %in% kids$mid), ]

    indx <- match(kids$mid, mothers$id)
    kids$marStart <- mothers$marStart[indx]
    kids$marEnd <- mothers$marEnd[indx]
    kids$m.birthdate <- mothers$birthdate[indx]
    kids <- kids[!is.na(kids$birthdate), ]
    tmp <- kids$id
    kids$id <- kids$mid
    kids$ch.id <- tmp
    kids$mid <- NULL
    kids <- kids[order(kids$id, kids$birthdate), ]
    kids$exit <- kids$birthdate - kids$m.birthdate
    kids$event <- TRUE
    kids <- rc(kids)
    kids$enter <- numeric(NROW(kids))
    kids$enter <- c(0, kids$exit[-NROW(kids)])
    
    kids$enter[kids$lopnr == 1] <- kids$marStart[kids$lopnr == 1]
    
    ## Kolla starten:
    pot <- kids$enter < kids$marStart & kids$ab == 1
    kids$marStart[pot] <- kids$enter[pot]
    marS <- with(kids, tapply(marStart, id, min))
    indx <- with(kids, tapply(id, id))
    kids$marStart <- marS[indx]
    ## Kolla slutet:
    put <- kids$exit > kids$marEnd & kids$ab == 1
    kids$marEnd[put] <- kids$exit[put] 
    marE <- with(kids, tapply(marEnd, id, max))
    indx <- with(kids, tapply(id, id))
    kids$marEnd <- marE[indx]
    ## Now, remove births outside (marStart, marEnd):
   
    kids <- kids[kids$enter >= kids$marStart & kids$exit <= kids$marEnd, ]
    
    kids <- rc(kids)
    ## return(kids)  ## not OK.............................not OK
    ## Now get best socst (= lowest) for married:
    married$socst[is.na(married$socst)] <- 9
    bestSocst <- with(married, tapply(socst, id, min))
    is.na(bestSocst) <- bestSocst == 9
    ##return(bestSocst)
    ## Put on last interval on 'kids':
    liv <- kids[kids$lopnr == kids$antrec, ]
    liv$event = FALSE
    liv$enter <- liv$exit
    liv$exit = liv$marEnd

    liv$ab <- liv$birthdate <- liv$deathdate <- liv$ch.id <- NA
    liv$lopnr <- liv$lopnr + 1
    liv$antrec <- liv$lopnr
    kids <- rbind(kids, liv)
    kids <- kids[order(kids$id, kids$birthdate), ]
    kids <- rc(kids)
    ## return(kids)   ### OK här !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ## And now put on childless married women (nomothers):
    ##return(nomothers)
    nm <- NROW(nomothers)
    nomo <- data.frame(id = nomothers$id,
                       region = nomothers$region,
                       ab = rep(NA, nm),
                       urban = nomothers$urban,
                       birthdate = rep(NA, nm), 
                       deathdate = rep(NA, nm),
                       marStart = nomothers$marStart,
                       marEnd = nomothers$marEnd, 
                       m.birthdate = nomothers$birthdate,
                       ch.id = rep(NA, nm),
                       exit = nomothers$marEnd,
                       event = rep(FALSE, nm),
                       lopnr = rep(1, nm),
                       antrec = rep(1, nm),
                       enter = nomothers$marStart)
    ##return(kids) ## OK!!
    ##return(nomo)
    kids <- rbind(kids, nomo)
    kids <- kids[order(kids$id, kids$lopnr), ]
    ## And put on 'bestSocst':
    indx <- match(kids$id, names(bestSocst))
    kids$socst <- bestSocst[indx]
    ## Swap on kids: birthdate --> ch.birthdate, m.birthdate --> birthdate
    kids$ch.birthdate <- kids$birthdate
    kids$birthdate <- kids$m.birthdate
    kids$m.birthdate <- NULL
    ##return(kids)  !! NOT ok!
    kids <- kids[!is.na(kids$socst), 
                 c("id", "ch.id", "region", "urban", "birthdate", "ch.birthdate", "deathdate",
                   "enter", "exit", "event", "socst", "marStart", "marEnd")]
    kids <- kids[order(kids$id, kids$ch.birthdate), ]
    kids <- rc(kids)
    oj <- kids$enter >= kids$exit
    kids$enter[oj] <- kids$exit[oj] - 0.003
    ##cat(dim(kids), "\n")
    kids <- age.window(kids, c(15, 50))
    ##cat(dim(kids), "\n")
    kids <- rc(kids)
    ## Now, fix hisclass:
    hc <- numeric(NROW(kids))
    hc[kids$socst %in% 1:2] <- 1
    hc[kids$socst == 3] <- 3
    hc[kids$socst == 4] <- 2
    hc[kids$socst == 5] <- 4
    hc[kids$socst == 6] <- 5
    hc[kids$socst == 7] <- 7
    hc[kids$socst == 8] <- 6
    kids$hisclass <- hc
    
    sb <- numeric(NROW(kids))
    sb[kids$socst %in% c(2, 5)] <- 1 #"official"
    sb[kids$socst %in% c(3, 7)] <- 2 #"farming"
    sb[kids$socst %in% c(1, 4)] <- 3 #"business"
    sb[kids$socst %in% c(6, 8)] <- 4 #"worker"
    
    kids$socBranch <- factor(sb, labels = c("official", "farming", "business", "worker"))
    save(kids, file = "data/kids.rda")
}
