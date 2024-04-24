# factor correlations
factanal.faccor <- function(x) {
    # x is fit from factanal
    tmat <- solve(x$rotmat)
    tmat %*% t(tmat)
}



factanal.loadings <- function(x, 
                               factor_order = seq(ncol(x$loadings)),
                                                  digits = 2) {
    # General utility function 
    # x is ft from fact anal
    l <- unclass(x$loadings)
    l <- l[,factor_order]
    rl <- round(l, digits)
    rl

}
