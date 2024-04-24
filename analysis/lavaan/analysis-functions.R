get_fit <- function(constraints = "strong", 
                    evaluative_factor = TRUE,
                    correlated_residuals = TRUE,
                    test = "hexaco200",
                    item_count = 10,
                    estimator =  "default") {
    fitid <- 
        models[ models$constraints == constraints &
                    models$evaluative_factor == evaluative_factor &
                    models$correlated_residuals == correlated_residuals &
                    models$test == test & 
                    models$item_count == item_count &
                    models$estimator == estimator, "id"]
    
    fits[[fitid]]
}


meanabs_lavaan_domaincor <- function(fit) {
    sp <- fit$sp
    spr <- sp[sp$lhs %in% v$hexaco_factors & sp$rhs %in% v$hexaco_factors, ]
    spr <- spr[spr$lhs != spr$rhs, ]
    sapply(split(spr, spr$groupname), function(X) mean(abs(X$est.std)))
}


cohensd_lavaan_evaluative_factor <- function(fit) {
    sp <- fit$sp
    spr <- sp[sp$lhs == "global", ]
    result <- spr[spr$op == "~1" & spr$groupname == "applicant", "est.std"]
    ifelse(length(result) == 0, NA, result)
}        
    
meanabscohensd_lavaan <- function(fit, factors = v$hexaco_factors) {
    sp <- fit$sp
    spr <- sp[sp$lhs %in% factors & sp$op == "~1", ]
    spr <- spr[spr$groupname == "applicant", ]
    result <- mean(abs(spr$est.std))
    ifelse(length(result) == 0, NA, result)
}        

meandhxac_lavaan <- function(fit) {
    sp <- fit$sp
    spr <- sp[sp$lhs %in% c("honestyhumility",  "extraversion", "agreeableness", 
                            "conscientiousness") & sp$op == "~1", ]
    spr <- spr[spr$groupname == "applicant", ]
    result <- mean(spr$est.std)
    ifelse(length(result) == 0, NA, result)
}

# fit <- cmodels$fit[[which(cmodels$test == "hexaco200")[3]]]
# I'm not quite happy with this.
# meanabscohensd_lavaan_facets <- function(fit) {
#     sp <- fit$sp
#     
#     if(sum(sp$rhs %in% v$hexaco_facets) == 0) return(NA)
#     
#     facetd <- sp[sp$lhs %in% v$hexaco_facets & sp$op == "~1" & 
#                    sp$groupname == "applicant", c("lhs", "est.std")]
#     names(facetd) <- c("facet", "rawd")
#     
#     facetloadings <- sp[sp$lhs %in% v$hexaco_factors & 
#                      sp$rhs %in% v$hexaco_facets  & 
#                      sp$groupname == "applicant", c("lhs", "rhs", "est.std")]
#     names(facetloadings) <- c("factor", "facet", "loading")
#     
#     domaind <- sp[sp$lhs %in% v$hexaco_factors & sp$op == "~1" & 
#                      sp$groupname == "applicant", c("lhs", "est.std")]
#     names(domaind) <- c("factor", "domaindd")
#     
#     facetloadings <- merge(facetloadings, domaind)
#     facetloadings$dfd <- facetloadings$loading * facetloadings$domaindd
#     dfd <- aggregate(dfd ~ facet, facetloadings, sum)
#     
#     comb  <- merge(facetd, dfd)
#     comb$totald <- comb$rawd + comb$dfd
#     
#      mean(abs(comb$totald))
#     
# }        
