fertil <- function(){
    ## Must be run from '..'! (see save at bottom)
    require(eha)
    require(skel14)
    mothers <- obs[with(obs, birthdate > 1820 &
                            birthdate < 1870 & sex == "woman" &
                                bthPlace == "region"), ]
    mothers <- mothers[order(mothers$id, mothers$enter), ]
    mothers <- rc(mothers)
    mapp <- unique(mothers$id[mothers$exit > 15 & mothers$enter < 50])
    mothers <- mothers[mothers$id %in% mapp,
                       c("id", "birthdate", "enter", "exit", "event", "civst", "socpo")]
    
    ##mothers
    kids <- obs[with(obs,
                          !is.na(mid) & mid %in% mothers$id &
                              bthPlace == "region"), ]
    kids <- kids[order(kids$id, -kids$exit),
                           c("mid", "id", "birthdate", "parity", "sex",
                             "enter", "exit", "event")]
    kids <- kids[!duplicated(kids$id), ]
    kids <- kids[order(kids$mid, kids$birthdate), ]
    indx <- match(kids$mid, mothers$id)
    m.birthdate <- mothers$birthdate[indx]
    kids$m.age <- kids$birthdate - m.birthdate
    mothers <- age.window(mothers, c(15, 50))
    mothers <- rc(mothers)
    kids <- kids[kids$m.age > 15, ]
    kids$lopnr <- unlist(with(kids, tapply(mid, mid, function(x) 1:length(x))))
    indx <- tapply(kids$mid, kids$mid)
    kids$antrec <- tapply(kids$mid, kids$mid, length)[indx]
    weq <- logical(NROW(mothers))
    for (i in 1:NROW(mothers)){
        if (i %% 10000 == 0) cat("i = ", i, "\n")
        if (mothers$lopnr[i] > 1.5){
            if (mothers$civst[i] == mothers$civst[i-1]){
                weq[i-1] <- TRUE
                mothers$enter[i] <- mothers$enter[i-1]
            }
        }
    }
    mothers <- mothers[!weq, ]
    mothers <- rc(mothers)
    cat ("removed", sum(weq), "records.\n")
    kids$enter <- NULL
    kids$enter[2:NROW(kids)] <- kids$m.age[1:(NROW(kids) - 1)]
    kids$enter[kids$lopnr == 1] <- NA

    Enter <- with(mothers, tapply(enter, id, min))
    Exit <- with(mothers, tapply(exit, id, max))
    indx <- tapply(mothers$id, mothers$id)
    mothers$Enter <- Enter[indx]
    mothers$Exit <- Exit[indx]

    addk <- kids[kids$lopnr == kids$antrec, ]
    addk$enter <- addk$m.age
    indx <- match(addk$mid, mothers$id)
    addk$m.age <- mothers$Exit[indx]
    addk$m.age <- ifelse(addk$m.age <= addk$enter, addk$enter + 0.01, addk$m.age)

    kids <- rbind(kids, addk)
    kids <- kids[order(kids$mid, kids$m.age), ]

    kids$ch.id <- kids$id ## NOTE:
    kids$id <- kids$mid
    kids$mid <- NULL

    kids <- rc(kids)

    indx <- match(kids$id[is.na(kids$enter)], mothers$id) 
    kids$enter[is.na(kids$enter)] <- mothers$Enter[indx]

    mar.age <- data.frame(id = unique(mothers$id), age = rep(51, length(unique(mothers$id))))
    for (i in 1:NROW(mar.age)){
        if (any(mothers$civst[mothers$id == mar.age$id[i]] == "married")){
            ##cat("TRUE\n")
            mar.age$age[i] <- min(mothers$enter[mothers$id == mar.age$id[i] &
                                                    mothers$civst == "married"])
        }
    }
    kids$civst <- "unmarried"
    id <- kids$id
    for (i in 1:NROW(kids)){
        if (id[i] %in% mar.age$id){
            age <- mar.age$age[mar.age$id == id[i]]
            if (kids$enter[i] >= age){
                kids$civst[i] <- "married"
            }else if (kids$exit[i] > age){
                kids$civst[i] <- "giftas"
            }
        }
    }
    kids$parity[kids$lopnr < kids$antrec] <- kids$parity[kids$lopnr < kids$antrec] - 1
    kids$ch.exit <- kids$exit
    kids$exit <- kids$m.age
    kids$event <- as.numeric(kids$lopnr < kids$antrec)
    kids$event <- ifelse(kids$event == 1 & kids$sex == "woman", 2, kids$event)

    indx <- match(kids$id, mothers$id)
    kids$birthdate <- mothers$birthdate[indx]
    kids$civst <- ifelse(kids$civst == "giftas", "married", kids$civst)
    kids$civst <- factor(kids$civst)
    ##return(kids)
    kids <- kids[, c("id", "birthdate", "parity", "enter", "exit", "event", "civst",
                     "ch.id", "sex", "ch.exit")]

    is.na(kids$ch.id) <- kids$event < 0.5
    is.na(kids$sex) <- kids$event < 0.5
    is.na(kids$ch.exit) <- kids$event < 0.5
    
    cl <- mothers[!(mothers$id %in% kids$id), ]
    
    cl$parity <- 0
    cl$event <- 0
    cl$civst <- as.character(cl$civst)
    cl$civst <- ifelse(cl$civst == "prev.married", "married", cl$civst)
    cl$civst <- factor(cl$civst)
    cl$ch.id <- NA
    cl$sex <- NA
    cl$ch.exit <- NA
    cl <- cl[, c("id", "birthdate", "parity", "enter", "exit", "event", "civst",
                 "ch.id", "sex", "ch.exit")]
    ##invisible(list(kids = kids, cl = cl))
##    return(list(kids = kids, cl = cl))
    
    kids <- rbind(kids, cl)
    kids <- kids[kids$enter <= kids$exit, ]
    kids$exit[kids$enter >= kids$exit] <-
        kids$enter[kids$enter >= kids$exit] + 0.01
    kids <- kids[order(kids$id, kids$enter), ]
    kids <- rc(kids)
    
    save(kids, file = "kids.rda")
    invisible(kids)
}
