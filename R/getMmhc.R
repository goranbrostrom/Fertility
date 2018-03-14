getMmhc <- function(dat){
    ## Reurns (m, M) for hisclass
    ## wrt urban, marAge
    res <- matrix(0, ncol = 2, nrow = 6) # 6 hisclasses
    j <- 1
    for (i in levels(dat$hisclass)){
        fit <- glm(event ~ offset(log(N * exposure)) + V + urban + marAge,
                   data = dat[dat$hisclass == i, ], family = poisson)
        ##return(fit$coef[1:2])
        res[j, ] <- fit$coef[1:2]
        j <- j + 1
    }
    colnames(res) <- c("M", "m")
    rownames(res) <- levels(dat$hisclass)
    res[, 1] <- exp(res[, 1])
    res
}
