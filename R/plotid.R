plotid <- function(dat, id = 55){
    enter <- dat$enter[dat$id == id] ##+ dat$marAge[dat$id == id]
    exit <- dat$exit[dat$id == id] ##+ dat$marAge[dat$id == id]
    start <- dat$marStart[dat$id == id][1]
    n <- length(enter)
    plot(c(start, exit[1]), c(0, 0), type = "l", col = "blue",
         xlim = c(start, max(exit)),
         ylim = c(0, n), 
         xlab = "t (age)", ylab = "N(t)", axes = FALSE)
    ##axis(1, at = (c(start, 30, 40, 50)), labels = round(c(start, 30, 40, 50)), 
      ##   cex.axis = 0.85)
    axis(1, at = c(start, exit), labels = round(c(start, exit)), cex.axis = 0.85)
    axis(2)
    box()
    ##points(exit[1], 0, pch = "c", col = "blue")
    for(i in 2:n){
        lines(c(enter[i], exit[i]), c(i-1, i-1), col = "blue")
        ##points(exit[i], i-1, pch = "c", col = "blue")
    }
}
