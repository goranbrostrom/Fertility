getMm <- function(dat){
    ## Reurns (m, M) for socBranch
    ## wrt urban, marAge
    res <- matrix(0, ncol = 2, nrow = 4)
    j <- 1
    for (i in levels(dat$socBranch)){
        fit <- glm(event ~ offset(log(N * exposure)) + V + urban + marAge,
                   data = dat[dat$socBranch == i, ], family = poisson)
        ##return(fit$coef[1:2])
        res[j, ] <- fit$coef[1:2]
        j <- j + 1
    }
    colnames(res) <- c("M", "m")
    rownames(res) <- levels(dat$socBranch)
    res[, 1] <- exp(res[, 1])
    res
}