valueCT <- function(dat){
    ## dat == vTab.

    source("R/plotCThaz.R")
    first <- kTab[kTab$period == "1851-1880" & kTab$hisclass != "farmer", ]
    last <- kTab[kTab$period == "1931-1940" & kTab$hisclass != "farmer", ]
    
    plotta <- function(datan){
        fit10 <- glm(event ~ offset(log(exposure)) + age,
                     data = datan, family = poisson) 
        a <- fit10$coef[1:6]
        a[2:6] <- a[1] + a[2:6]
        y1 <- cumsum(c(0, exp(a))) * 5
        x <- seq(20, 50, by = 5)
        fit11 <- glm(event ~ offset(log(N * exposure)) + V,
                     data = datan, family = poisson)
        M <- exp(fit11$coef[1])
        m <- fit11$coef[2]
        y2 <- plotCThaz(M = M, m = m, fig = FALSE)
        head <- paste("M = ", round(exp(fit11$coef[1]), 2),
                      ", m = ", round(fit11$coef[2], 2)) 
        plot(x, y1, type = "l", lty = 1, main = head, xlab = "Age",
             ylab = "Cum. Hazards", ylim = c(0, 8.5), las = 1)
        axis(4, las = 1)
        lines(x, y2, col = "red", lty = 2)
    }

    oldpar <- par(mfrow = c(1, 2))
    plotta(first)
    legend("bottomright", legend = c("PCH", "CT"),
           lty = 1:2, col = c("black", "red"))
    plotta(last)
    legend("bottomright", legend = c("PCH", "CT"),
           lty = 1:2, col = c("black", "red"))
    par(oldpar)
    
}
