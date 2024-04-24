v$fitindicies <- c("npar",  "chisq", "df", "pvalue", "cfi", "tli", "rmsea", 
                   "rmsea.ci.lower", "rmsea.ci.upper", "srmr")

meta.hexaco <- readRDS("lavaan/meta.hexaco.rds")

extract_parameters <- function(fits) {
    sf <-standardizedsolution(fits)
    uf <- parameterestimates(fits)
    
    sfs <- split(sf, sf$group)
    names(sfs) <- c("research", "applicant")
    ufs <- split(uf, uf$group)
    names(ufs) <- c("research", "applicant")
    
    fsr <- sfs$research
    fsr$est.std <- NULL
    
    fsr$est.std_research <- sfs$research$est.std
    fsr$est.std_applicant <- sfs$applicant$est.std
    fsr$est_research<- ufs$research$est
    fsr$est_applicant <- ufs$applicant$est
    
    fsr$est.std_equal <- abs(fsr$est.std_research - fsr$est.std_applicant) < .0001
    fsr$est_equal <- abs(fsr$est_research - fsr$est_applicant) < .0001
    fsr$equal <- paste("est.std.eq", fsr$est.std_equal, "; est.et", fsr$est_equal)
    
    list(fsr = fsr, split_fsr = split(fsr, fsr$equal))
}

# organise meta data
get_items <- function(item_count = 3) {
    # mh <- meta.hexaco[ meta.hexaco$factor %in% v$hexaco_factors, ]
    mh <- meta.hexaco
    mh1 <- mh[ mh$reversed == 1, ]
    first_items <- mh1[!duplicated(mh1$facet), "id"]
    latter_items <- setdiff(mh$id, first_items)
    mh$selection_order <- match(mh$id, c(first_items, latter_items))
    mh <- mh[order(mh$selection_order), ]
    smh <- split(mh, mh$facet)
    smh <- lapply(smh, function(X) {
        X$order <- seq(nrow(X));
        X})
    emh <- unsplit(smh, mh$facet)
    emh <- emh[emh$order <= item_count, ]
}

get_items_domains <- function(item_count = 3, hexaco60 = FALSE) {
    # mh <- meta.hexaco[ meta.hexaco$factor %in% v$hexaco_factors, ]
    mh <- meta.hexaco
    mh <- mh[mh$factor != "other", ] # remove altruism
    
    if (hexaco60) {
        mh <- mh[!is.na(mh$hexaco60), ]
    }

    mh1 <- mh[ mh$reversed == 1, ]
    first_items <- mh1[!duplicated(mh1$factor), "id"]
    latter_items <- setdiff(mh$id, first_items)
    mh$selection_order <- match(mh$id, c(first_items, latter_items))
    mh <- mh[order(mh$selection_order), ]
    smh <- split(mh, mh$factor)
    smh <- lapply(smh, function(X) {
        X$order <- seq(nrow(X));
        X})
    emh <- unsplit(smh, mh$factor)
    emh <- emh[emh$order <= item_count, ]
    emh
}


getcorrelated_residuals <- function(x, threshold = 2) {
    lmod <- lavaan_indicators("global", names(x))
    lfit <- lavaan::cfa(lmod, x)    
    # there seemed a possibly a lavaan version issue that requires
    # removal of missing and 0 mi values
    ml <- na.omit(modificationindices(lfit))
    ml <- ml[ml$mi != 0, ]
    ml$mad <- (ml$mi - median(ml$mi)) / mad(ml$mi)
    # ml$rank <- nrow(ml) - rank(ml$mi, ties.method = "first") + 1
    ml$syntax <- paste(ml$lhs, ml$op, ml$rhs)
    # rml <- ml[ml$rank  <= threshold, ]
    rml <- ml[ml$mad >= threshold, ] 
    rml$syntax
}


estimate_model <- function(item_count = 3, # items per facet or domains
           hexaco60 = TRUE, # FALSE means HEXACO200
           orthogonal = TRUE, # correlations are explicitly defined
          correlated_residuals = FALSE, # whether to include correlated residuals
          group.equal = "", # general group equality constraints
          se = "none", verbose = TRUE, 
          threshold = 5, # threshold for included correlated residuals
          evaluative_factor = FALSE, # whether to include evaluative factor
          estimator = "default", # estimator; default is ML, see ?cfa
          fit_model = TRUE,
          return_fitobject = TRUE, # whether to return the fit object
                            # with large models, can save memory not to return
                            # object
          #, acquiescence_factor = FALSE
          id = "default", # a name for the model
          outputdir = NULL # if a location is provided, then write fit
                            # to this folder
          ) {
    # http://lavaan.ugent.be/tutorial/groups.html
    # Get items and meta data
    if (hexaco60) {
        emh <- get_items_domains(item_count, hexaco60 = hexaco60)
    } else {
        emh <- get_items(item_count)
    }
    
    # Declare model
    model <- ""

    if (hexaco60) {
        # Item loadings on Domains
        model <- lavaan_reordered_indicators(emh$factor, emh$id, ccases)
        
    } else {
        # Facet loadings on domains
        # Declared explicitly because of the complexity of including altruism
        model <- paste0("
honestyhumility =~ sincerity + fairness + greedavoidance + modesty + aload * altruism
emotionality =~ fearfulness + anxiety + dependence + sentimentality + aload * altruism
extraversion =~ socialselfesteem + socialboldness + sociability + liveliness
agreeableness =~ forgiveness + gentleness + flexibility + patience + aload * altruism
conscientiousness =~ organization + diligence + perfectionism + prudence
openness =~ aestheticappreciation + inquisitiveness + creativity + unconventionality")
        
        # Item loadings on facets
        model <- paste0(model, "\n\n", lavaan_indicators(emh$facet, emh$id) )
    }
    
    # Domain covariances
    model <- paste0(model, "\n\n", lavaan_covariances(v$hexaco_factors))
    
    # Evaluative factor
    if(evaluative_factor) {
        model <- paste0(model, "\n\n", 
            lavaan_reordered_indicators("global", emh$id, ccases, v$hexaco_big1))
    }
    
    # Correlated residuals
    if (correlated_residuals) {
        semh <- split(emh, emh$factor)
        cr <- lapply(semh, function(X) getcorrelated_residuals(ccases[,X$id], threshold = threshold))
        cr <- unlist(cr)
        crname <- gsub(" ~~ ", "", cr)
        crname <- paste0("c(", crname, ", ", crname, ") * ")
        cr <- sapply(seq(cr), function(i) gsub(" ~~ ", paste0(" ~~ ", crname[i]), cr[i]))
        crp <- paste0(cr, collapse = "\n")
        
        
        model <- paste0(model, "\n\n", crp, "\n")
    }
    
    # Removed because I could not get convergence
    # if (acquiescence_factor) {
    #     acq_term <- paste0("acquiescence =~ ", paste0("c(acquiescence_loading, acquiescence_loading) * ", emh$id, collapse = " + "))
    #     acq_var <- "acquiescence ~~  start(0.01, 0.01) * acquiescence"
    #     m_higherorder <- paste0(m_higherorder,  "\n\n", acq_term, "\n", acq_var)
    # }
    # cat(m_higherorder)
    
    
    
    
    if (!is.null(outputdir)) {
        dir.create(outputdir, showWarnings = FALSE)
        status_file <- 
            paste0("estimation-commenced-", id, "-", format(Sys.time(), "%a-%b-%d-%H%M%S-%Y"))
        status_file <- file.path(outputdir, status_file)
        file.create(status_file)
    }
    
    if (fit_model) {
        fit <- try(lavaan::cfa(model, 
                                 data = ccases[,c(emh$id, "sample")], 
                                 group = "sample", 
                                 group.equal = group.equal,
                                 estimator = estimator
                                 # c("loadings" # necessary to ensure facets and factors have the same meaning
                                 #                 # , "residuals"
                                 #                 #, "lv.covariances",
                                 #                 , "lv.variances"
                                 #                 , "intercepts" # necessary for estimating item means
                                 #                 # there are no "residual.covariances", "regressions"
                                 #                             )
                                 ,
                                 
                                 se = se, 
                                 orthogonal = orthogonal, verbose = verbose))
        
    } else {
        fit <- "nofit requested"
    }
    sp <- try(standardizedsolution(fit))
    up <- try(parameterestimates(fit))
    fs <- try(fitmeasures(fit, v$fitindicies))
    
    # Model specification and convergence details
    detail_names <- c("group.label", "nobs", "iterations",
                     "call", "options", "timing", "test",
                  "post.check", "converged")
    details <- lapply(detail_names, function(X) try(inspect(fit, X)))
    names(details) <- detail_names
    
    # Imputed matrices and vectors
    imp_names <- c("cov.ov", "cor.ov", "mean.ov", "cor.all")
    imp <- lapply(imp_names, function(X) try(inspect(fit, X)))
    names(imp) <- imp_names
    
    if (!return_fitobject) {
        fit <- "fit object removed"
    }
    
    obj <- list(items = emh, model = model, fit = fit, sp = sp, up = up, fs = fs,
         details = details, imp = imp)
    
    if (!is.null(outputdir)) {
        status_file <- 
            paste0("estimation-commenced-", id, "-", format(Sys.time(), "%a-%b-%d-%H%M%S-%Y"),
                   ".rdata")
        status_file <- file.path(outputdir, status_file)
       save(obj, file = file.path(status_file))
    }
    
    obj
}



