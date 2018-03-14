plotCThaz <- function(M = 1, m = 0, what = c("cumhaz", "haz"), fig = TRUE){
    ## CT = Coale-Trussell model
    what <- what[1]
    N <- c(.460, .431, .395, .322, .167, .024)
    V <- c(0.0, -0.279, -0.667, -1.042, -1.414, -1.671)
    lambda <- numeric(6)
    lambda <- N * M * exp(m * V)
    x <- seq(20, 50, by = 5)
    H <- cumsum(c(0, lambda)) * 5
    if (fig){
        par(mfrow = c(1, 2))
        plot(x, H, type = "l", col = "blue", xlab = "age", ylab = "Cum. hazard")
        abline(h = 0)
        plot(x[1:2], c(lambda[1], lambda[1]), col = "blue", 
             type = "l", xlim = c(20, 50), ylim = c(0, max(lambda)),
            xlab = "Age", ylab = "Hazard")
        for (i in (2:6)){
            lines(x[i:(i+1)], c(lambda[i], lambda[i]), col = "blue")
        }
        abline(h = 0)
    }
    invisible(H)
}