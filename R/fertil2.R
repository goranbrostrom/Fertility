 fertil2 <- function(){
    ## Expansion of 'fertil'; uses 'skum' instead of 'skel14'.
    ## Must be run from '..'! (see save at bottom)
    ## Note also that we include Umeå and start 1800.99 instead of 1820.99! 
    
    library(skum) # loads 'eha' as well...
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

    kids <- skum::per[skum::per$mid %in% married$id, c("id", "mid", "region", "foddat", "doddat", "ab", "fodhfrs")]
    kids$urban <- kids$fodhfrs %in% c(82780, 82981) # Added 10 oct 2016
    kids$fodhfrs <- NULL
    kids <- kids[kids$ab %in% c(0, 1, 3), ]
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
    ## return(kids)
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
    kids <- rbind(kids, nomo)
    kids <- kids[order(kids$id, kids$lopnr), ]
    ## And put on 'bestSocst':
    indx <- match(kids$id, names(bestSocst))
    kids$socst <- bestSocst[indx]
    ## Swap on kids: birthdate --> ch.birthdate, m.birthdate --> birthdate
    kids$ch.birthdate <- kids$birthdate
    kids$birthdate <- kids$m.birthdate
    kids$m.birthdate <- NULL
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