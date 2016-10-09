ageGroup <- function(dat, ages){
    n <- length(ages) - 1
    ##age <- ages[-(n+1)]
    if (n > 1){
        library(eha)
        dat$age <- numeric(NROW(dat))
        first <- age.window(dat, ages[1:2])
        first$age <- ages[1]
        for (i in 2:n){
            tmp <- age.window(dat, ages[i:(i+1)])
            tmp$age <- ages[i]
            first <- rbind(first, tmp)
        }
    }
    first <- first[order(first$id, first$enter), ]
    invisible(first)
}