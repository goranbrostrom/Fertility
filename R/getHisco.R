getHisco <- function(){
    ## Reading from _two_ .xlsx files:
    library(readxl)
    f1 <- read_excel("data/TYKOD_HISCO_160523.xlsx")
    cat("dim(f1) = ",  dim(f1), "\n")
    f1 <- f1[!(f1$HISCOKOD == "<null>"), ]
    cat("dim(f1) = ", dim(f1), "\n")
    f1$HISCOKOD <- as.numeric(f1$HISCOKOD)
    f2 <- read_excel("data/HISCO2.xlsx", skip = 1)
    f2 <- f2[, 1:3]
    f2
    hiscoCode <- rbind(f1, f2)
    hiscoCode <- hiscoCode[!duplicated(hiscoCode$TYKOD), ]
    save(hiscoCode, file = "data/hiscoCode.rda")
    cat("Saved in 'data/hiscoCode.rda\n")
}