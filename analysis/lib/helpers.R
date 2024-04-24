helper.function <- function()
{
  return(1)
}

quadratic_regression <- function(dv, iv, data = ccases) {
    f1 <- as.formula(paste0(dv, " ~ ", iv))
    f2 <- as.formula(paste0(dv, " ~ ", iv, " + I(", iv, "^2)"))
    fit1 <- lm(f1, data)
    fit2 <- lm(f2, data)
    sfit1 <- summary(fit1)
    sfit2 <- summary(fit2)
    c(lin = sfit1$adj, quad = sfit2$adj - sfit1$adj)
}

round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    
    df[,nums] <- round(df[,nums], digits = digits)
    
    (df)
}




groupdiff <- function(X = ccases[ , v$hexaco_facets], groupvar = ccases$sample) {
    desfacet <- data.frame(
        mean = sapply(split(X, groupvar), function(X) sapply(X, mean)),
        sd =  sapply(split(X, groupvar), function(X) sapply(X, sd)),
        relativesd =  sapply(split(X, groupvar), 
                             function(X) sapply(X, relativeSD, 1, 5))
    )
    desfacet$diff <- desfacet$mean.applicant - desfacet$mean.research
    desfacet$pooled_sd <- sqrt((desfacet$sd.applicant^2 +  desfacet$sd.research^2)/2)
    desfacet$cohensd <- desfacet$diff / desfacet$pooled_sd
    
    # p value for group difference
    dvs <- names(X)
    xc <- data.frame(X, groupvar)
    fits <- lapply(dvs, function(X) regression(X,  "groupvar", xc))
    names(fits) <- dvs
    desfacet$p <-  sapply(fits, function(X) anova(X)["groupvar", "Pr(>F)"])
    # desfacet$stars <- as.character(gtools::stars.pval(desfacet$p))
    # used local function  to remove dependency on gtools
    desfacet$stars <- as.character(stars.pval(desfacet$p))
        
    
    desfacet
}

cormatmsd <- function(x, digits = 2) {
    corx <- round(cor(x), digits)
    mx <- round(apply(x, 2, mean), digits)
    sdx <- round(apply(x, 2, sd), digits)
    data.frame(mean = mx, sd = sdx, corx)
}

minimum_significant_r <- function(n, alpha=.05, twotail=TRUE, precision=.01) {
    # calculate minimum significant correlation
    # useful to report at bottom of correlation matrices instead of using stars
    # n: sample size
    # alpha: alpha level for determining statistical significance
    # twotail: TRUE means two tailed significance; 
    #          FALSE means one tailed significance
    # precision: precision of significant r (typically .01, .001, or .0001)
    
    r <- seq(0, 1, by=precision)
    tvalue <- r * sqrt((n-2)/(1-r^2))
    pvalue <-  1 - pt(tvalue,  df=n-2)
    if(twotail)  pvalue <- pvalue * 2 
    first <- min(which(pvalue < alpha))
    r[first]
}


topbottomr <- function(lower, upper) {
    r <- lower
    r[upper.tri(r)] <- upper[upper.tri(r)]
    diag(r) <- NA
    r
    
}
modal_response <- function(x) {
    tab <- table(x) 
    as.numeric(names(tab[which.max(tab)]))
}

# http://stackoverflow.com/questions/9063889/how-to-round-a-data-frame-in-r-that-contains-some-character-variables
round_df <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
    
    df[,nums] <- round(df[,nums], digits = digits)
    
    (df)
}


swr = function(string, nwrap=20) {
    paste(strwrap(string, width=nwrap), collapse="\n")
}


# taken from gtools package
# placed here to remove a dependency
stars.pval <- function (p.value) 
{
    unclass(symnum(p.value, corr = FALSE, na = FALSE, cutpoints = c(0, 
              0.001, 0.01, 0.05, 0.1, 1), symbols = c("***", "**", 
                                                                                                            "*", ".", " ")))
}
