

# lavaan model definition utility functions
lavaan_indicators <- function(factors, indicators) {
    # creates factors with indicators 
    # Typically, one of the following holds:
    # (a) a single factor is supplied with multiple indicators
    # (b) a vector of factors of equal length as the indicators is 
    #     supplied where the factors correspond in order to the indicators
    ufactors <- unique(factors)
    modstr <- sapply(ufactors, function(X) paste(X, "=~", 
                                                 paste(indicators[factors == X], collapse = " + ")))
    paste(paste(modstr, collapse = "\n"), "\n")
}


lavaan_reordered_indicators <- function(factors, indicators, data, score_variables = NULL) {
    # function that ensures that first item is the item with the largest
    # positive correlation with the factor
    
    reorderonefactor <- function(fac, ind, data, score_variable = NULL) {
        if (is.null(score_variable)) {
            rs <- as.numeric(cor(data[,fac], data[,ind]))
        } else {
            rs <- as.numeric(cor(data[,score_variable], data[,ind]))
        }
            
        names(rs) <- ind
        # check that there is at least one positive loading
        # otherwise, measure needs to be re-ordered
        # or possibly negative loading constraint could be applied
        stopifnot(sum(rs > 0) > 0)
        
        focal_indicator <- names(which.max(rs))
        reordered_ind <- c(focal_indicator, setdiff(ind, focal_indicator))
        paste0(fac, " =~ ",  paste(reordered_ind, collapse = " + "))
    }

    ufactors <- factors[!duplicated(factors)]
    if(is.null(score_variables)) {
        modstr <- sapply(ufactors, function(X) 
            reorderonefactor(X, indicators[factors == X], ccases))
    } else {
        uscore_variables <- score_variables[!duplicated(factors)]
        names(uscore_variables) <- ufactors
        modstr <- sapply(ufactors, function(X) 
            reorderonefactor(X, indicators[factors == X], ccases, uscore_variables[X]))
    }
    
    paste(paste(modstr, collapse = "\n"), "\n")
}
    
    
# lavaan model definition utility functions
lavaan_indicators <- function(factors, indicators) {
    # creates factors with indicators 
    # Typically, one of the following holds:
    # (a) a single factor is supplied with multiple indicators
    # (b) a vector of factors of equal length as the indicators is 
    #     supplied where the factors correspond in order to the indicators
    ufactors <- unique(factors)
    modstr <- sapply(ufactors, function(X) paste(X, "=~", 
                                                 paste(indicators[factors == X], collapse = " + ")))
    paste(paste(modstr, collapse = "\n"), "\n")
}


lavaan_covariances <- function(set1) {
    # create all covariances between variables listed
    copyset <- set1
    modstr <- list()
    for(i in seq(1, length(set1) - 1)) {
        copyset <- setdiff(copyset, set1[i])
        modstr[[i]] <- paste(set1[i], "~~", paste(copyset, collapse = " + "))
        
    }
    paste(paste(modstr, collapse = "\n"), "\n")
}



lavaan_crosscovariances <- function(set1, set2) {
    # create all cross-covariances between set1 and set2
    # where set1 and set2 are different
    stopifnot(length(intersect(set1,set2)) == 0)
    modstr <- lapply(set1, function(X) paste(X, "~~", paste(set2, collapse = " + ")))
    paste(paste(modstr, collapse = "\n"), "\n")
}

lavaan_fakelatent <- function(set) {
    modstr <- paste0("fac_", set, " =~", set)
    paste(paste(modstr, collapse = "\n"), "\n")
}

