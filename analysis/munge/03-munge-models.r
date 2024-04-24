# predicated on having run "lavaan-models.rmd"
# or using an existing version of them.
source("lavaan/model-functions.R")
source("lavaan/analysis-functions.R")
load("output/hexaco60fits.rdata")
load("output/model-fits.rdata")
load("lavaan/models.rdata")


row.names(models) <- models$id
models$constraints_factor <- 
    factor(models$constraints, c("configural", "weak", "strong", 
                                 "strong_variances", "strong_covariances",
                                 "strict", "variances", "covariances"))
models$all_items <- models$item_count == 10
# combine hexaco200 and hexaco60 fits
rfits <- c(fits, hexaco60fits)

# currently available models
rmodels <- models[models$id %in% names(rfits), ]
# add fit object to rmodels
# this creates some issues with printing rmodels but makes it easier to reliable add
# information to rmodels
rmodels$fit <-  sapply(rmodels$id, function(X) rfits[[X]], simplify = FALSE, USE.NAMES = TRUE)

# main models (main models for tables)
rmodels$main_models <- (rmodels$all_items == TRUE & 
                            rmodels$estimator == "default" &
                            rmodels$correlated_residuals == TRUE)


# function that removes the fit object for quick printing purposes
rmf <- function(x) { x[, setdiff(names(x) , "fit")]}


# add convergence status to model description
rmodels$converged <- sapply(rmodels$fit, function(X) X$details$converged)

# overview of model convergence status
table(rmodels$converged) # how many models converged
rmodels[ !rmodels$converged, "id"] # non converging ids

# cleaned models
cmodels <- rmodels[ rmodels$converged == TRUE, ]

# add labels to group names
for (i in seq(nrow(cmodels))) {
    cmodels$fit[i][[1]]$sp$groupname <- 
        factor(cmodels$fit[i][[1]]$sp$group, 1:2, c("research", "applicant"))
    cmodels$fit[i][[1]]$up$groupname <- 
        factor(cmodels$fit[i][[1]]$up$group, 1:2, c("research", "applicant"))
}

# add fit statistics
fs <- data.frame(t(sapply(cmodels$fit, function(X) X$fs)))
cmodels[ , names(fs)] <- fs
# head(rmf(cmodels))

# add meanabsr
macor <- data.frame(t(sapply(cmodels$fit , meanabs_lavaan_domaincor)))
names(macor) <- paste0("mabsr_", names(macor))
cmodels[, names(macor)] <- macor

# add evaluative mean d
cmodels$dglobal <- sapply(cmodels$fit , cohensd_lavaan_evaluative_factor)

# add mean abs domain d
cmodels$mabsd_domains <- sapply(cmodels$fit, function(X) 
    meanabscohensd_lavaan(X, v$hexaco_factors))

cmodels$meand_hxac <- sapply(cmodels$fit, function(X) 
    meandhxac_lavaan(X))

# just hexaco60 models
cm60 <- cmodels[ cmodels$test == "hexaco60", ]

