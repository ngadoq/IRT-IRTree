meanabscor <- function(x) {
    corx <- cor(x)
    mean(abs(corx[lower.tri(corx)]))
}

# proportion variance explained by first unrotated principal component
propfc<- function(x) {
    pc <- psych::principal(x, 1)
    sum(pc$communality) / ncol(x)
}
    

global_factor <- function(data) {
    c(
        meanabscor_items = meanabscor(data[,v$hexaco_items]),
        meanabscor_facets = meanabscor(data[,v$hexaco_facets]),
        meanabscor_factors = meanabscor(data[,v$hexaco_factors]),
        propfc_items = propfc(data[,v$hexaco_items]),
        propfc_facets = propfc(data[,v$hexaco_facets]),
        propfc_factors = propfc(data[,v$hexaco_factors])
    )
        
}
