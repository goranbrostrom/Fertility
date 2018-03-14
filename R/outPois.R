outPois <- function(){
  x <- matrix(scan("data/poisout.txt", what = character()), ncol = 5, byrow = TRUE)
  colnames(x) <- x[1, ]
  x <- x[-1, ]
  rownames(x) <- c("log(M)    ",
                   "V         ",
                   "urban     ",
                   "marAge    ",
                   "   (20,25]",
                   "   (25,30]",
                   "   (30,35]",
                   "   (35,50]")
  x[, -(1:2)]
}
  