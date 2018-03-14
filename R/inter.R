inter <- function(bdate, farmer, bf){
    ## farmer
    f <- function(tid) exp(farmer + bdate * tid + bf * tid)
    x <- seq(-30, 20, length = 100)
    fx <- f(x)
    ret <- cbind(x, fx)
    colnames(ret) <- c("time", "RR")
    ret
}
