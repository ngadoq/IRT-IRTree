# This code is from the relativeVariability package.
# Merijn Mestdagh (2016). relativeVariability: Relative Variability. R package version 1.0.
# Because the package is not yet on cran, the code was reproduced here
# to make the package of data and code more reproducible

relativeSD <- function (X, MIN, MAX) 
{
    M <- mean(X)
    SD <- sd(X)
    checkInput(X, MIN, MAX)
    n <- length(X)
    mv <- maximumVAR(M, MIN, MAX, n)
    msd <- sqrt(mv)
    if (msd != 0) {
        rsd = SD/msd
    }
    else {
        rsd <- NaN
    checkOutput(M, MIN, MAX)
    }
    relativeSD <- rsd
}


maximumVAR <- function (M, MIN, MAX, n) 
{
    if (M == MIN || M == MAX) {
        mv = 0
    }
    else {
        if (abs(MIN) > abs(MAX)) {
            MINt <- -MAX
            MAX <- -MIN
            MIN <- MINt
            M <- -M
        }
        nMax <- floor((n * M - n * MIN)/(MAX - MIN))
        nMin <- n - 1 - nMax
        if (nMax == 0) {
            MAX <- 0
        }
        m <- n * M - nMin * MIN - nMax * MAX
        mv <- (nMin * (MIN - M)^2 + nMax * (MAX - M)^2 + (M - 
                                                              m)^2)/(n - 1)
    }
    maximumVar = mv
}


checkOutput <- function (M, MIN, MAX) 
{
    if (M == MAX) {
        warning("NaN returned. Data has a mean equal the maximum")
    }
    else if (M == MIN) {
        warning("NaN returned. Data has a mean equal the minimum")
    }
    else {
        warning("NaN returned.")
    }
}

checkInput <- function (X, MIN, MAX) 
{
    if (length(which(X > MAX)) > 0) {
        stop("Values found bigger than the maximum")
    }
    else if (length(which(X < MIN)) > 0) {
        stop("Error, MIN>MAX")
    }
    else if (MIN > MAX) {
        stop("Error, MIN>MAX")
    }
}