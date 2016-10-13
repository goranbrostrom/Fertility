tmfrCT <- function(M, m, age = 50, 
                   N = c(0.460, 0.431, 0.395, 0.322, 0.167, 0.024), 
                   V = c(0, -0.279, -0.667, -1.042, -1.414, -1.671)){
    ## We want the cumulative hazard at 'age': H(age), 20 < age <= 50.
    ## N, V are given by Coale & Trussell.
    lambda <- M * N * exp(m * V)
    H <- cumsum(c(0, lambda)) * 5
    jumps <- seq(20, 50, by = 5)
    if (age <= 20) error("Too small age!")
    if (age <= 25){
        res <- (age - 20) * lambda[1]
    }else if (age <= 30){
        res <- H[2] + (age - 25) * lambda[2]
    }else if (age <= 35){
        res <- H[3] + (age - 30) * lambda[3]
    }else if (age <= 40){
        res <- H[4] + (age - 35) * lambda[4]
    }else if (age <= 45){
        res <- H[5] + (age - 40) * lambda[5]
    }else if (age <= 50){
        res <- H[6] + (age - 45) * lambda[6]
    }else{
        error("Too large age!")
    }
    
    res
}