# Example preprocessing script.
# library(ProjectTemplate); load.project(list(munging=FALSE)) # use to debug munging file


# drop altruism
# meta.hexaco <-meta.hexaco[!meta.hexaco$facet == "altruism", ]

v <- list()
v$hexaco_items <- meta.hexaco$id
# v$scwb_items <- meta.cwb$id
v$all_items <- c(v$hexaco_items, v$scwb_items)
v$hexaco_facets <- meta.hexacofacets[ , 'id'] # excludes altruism
v$hexaco_factors <- unique(meta.hexacofacets[ meta.hexacofacets$core, 'factor'])
v$hexaco_big5factors <- meta.hexacofactors[ meta.hexacofactors$big5, 'factor']
v$hexaco_big5facets <- meta.hexacofacets[ meta.hexacofacets$big5, 'id']
v$hexaco_big1 <- "hexaco_global_ml"
# v$scwb_scales <- unique(c(meta.cwb$total, meta.cwb$scale))




create_scoring_key <- function(items, scales, reverse) {
    unique_scales <- unique(scales)
    key <- sapply(seq(unique_scales), 
                  function(X) ifelse(scales == unique_scales[X], reverse, 0))
    key <- data.frame(key)
    names(key) <- unique_scales
    row.names(key) <- items
    key
}

score_test <- function(meta_data, case_data, subscale_name='subscale_name', id='id', reverse='reverse') {
    scoring_key <- create_scoring_key(meta_data[, id], 
                                      meta_data[ ,subscale_name],  
                                      meta_data[ ,reverse])
    scored <- scoreItems(as.matrix(scoring_key), case_data[,rownames(scoring_key)])
    scored$key <- scoring_key
    scored
}


scorealltests <- function(data = rcases) {
    scored <- list()
    scored$hexacofacets <- score_test(meta.hexaco, data, subscale_name = "facet", reverse="reversed")
    scored$hexacofactors <- score_test(meta.hexaco, data, subscale_name = "factor", reverse="reversed")
    # scored$scwb <- score_test(meta.cwb, data, subscale_name = "scale", reverse="reversed")
    # scored$scwbtotal <- score_test(meta.cwb, data, subscale_name = "total", reverse="reversed")
    scored
}

scored <- scorealltests()


add_scores <- function(data, scored = scored) {
    for (i in names(scored)) {
        data[,colnames(scored[[i]]$scores)] <- scored[[i]]$scores[,]
    }
    data
}

rcases <- add_scores(rcases, scored)



#############################################################
# Remove unmatched cases.
rcases$matched <- rcases$id %in% ecases$id
ccases <- rcases[ rcases$matched , ]

# Correlation between loading on global factor and faking


##############################################################
# Measures of social desirability
# General factor of personality

# First unrotated factor: Maximum likelihood using factors
facg <- factanal(ccases[,v$hexaco_factors], 1, scores = "regression")
ccases$hexaco_global_ml <-  as.numeric(facg$scores)

# First unrotated principal component 
pcg <- principal(ccases[,v$hexaco_factors], 1)
ccases$hexaco_global_pca <-  pcg$scores


# Unit weighted composite of the six domains (emotionality reversed)
ccases$hexaco_global_unit <- as.matrix(ccases[,
    c("honestyhumility", "emotionality", "extraversion", "agreeableness", 
    "conscientiousness",  "openness")]) %*%  c(1, -1, 1, 1, 1, 1)


# First unrotated factor: Maximum likelihood using items
faci <- factanal(ccases[,v$hexaco_items], 1, scores = "regression")
ccases$hexaco_items_global_ml <-  as.numeric(0 - faci$scores)


# Distance and proximity from mean
ccases$mahalanobis_distance_applicant <- 
    mahalanobis(ccases[,v$hexaco_items], 
                center = sapply(ccases[ccases$sample == "applicant",
                                       v$hexaco_items], mean), 
                cov = cov(ccases[ccases$sample == "applicant",
                                 v$hexaco_items]))
ccases$mahalanobis_distance_research <- 
    mahalanobis(ccases[,v$hexaco_items], 
                center = sapply(ccases[ccases$sample == "research",
                                       v$hexaco_items], mean), 
                cov = cov(ccases[ccases$sample == "research",
                                 v$hexaco_items]))

ccases$proximity_to_ideal_applicant <- 0 - ccases$mahalanobis_distance_applicant
ccases$proximity_to_ideal_research <- 0 - ccases$mahalanobis_distance_research


# Distance and proximity from mode
ccases$modal_distance_applicant <- modal_distance(
    ccases[,v$hexaco_items],
    ccases[ ccases$sample == "applicant", v$hexaco_items])
    
ccases$modal_proximity_applicant <- 0 -ccases$modal_distance_applicant
    

# Proportion of responses that are Strongly disagree or strongly agree
ccases$prop_extreme <- apply(ccases[,v$hexaco_items], 1, function(X) mean(X %in% c(1,5)))


# Social deirability

des <- items_option_desirability()
itemtable <- des$destab
ccases$weighted_desirability <- des$weighted_desirability

# long item table
dit <- data.frame(itemtable)
dit <- data.frame(text = meta.hexaco$text, dit)


dit$item <- row.names(dit)
longitemtable <- reshape(dit, varying = paste0("X", 1:5), direction = "long", 
                   v.names = "desirability",
                   timevar = "option",
                   times = 1:5,
                   new.row.names = 1:10000)
longitemtable$item <- factor(longitemtable$item, levels = v$hexaco_items)

# more item level processing
sli <- split(longitemtable, longitemtable$id)
slitab <- data.frame(id = as.numeric(names(sli)), 
                     linear = sapply(sli, function(X) coef(lm(desirability ~ option, X))["option"])
)
slitab

compare_strongly <- function(x, positive = TRUE) {
    # determine the positively desirable pole
    # and calculate the degree to which strongly agree or strongly disagree is larger
    # than agree or disagree, where the choice between agree pole or disagree pole is 
    # determine by the linear trend of social desirability
    if (positive) {
        x[x$option == 5, "desirability"] - x[x$option == 4, "desirability"]
    } else {
        x[x$option == 1, "desirability"] - x[x$option == 2, "desirability"]
    }
}

slitab$strongly_increase <-sapply(seq(nrow(slitab)), function(X) 
    compare_strongly(sli[[X]], slitab[X, "linear"]> 0))


slitab$positive <- slitab$linear > 0

# use roughly half the average absolute linear increase per scale option
# to classify agree and strongly agree as similar
# mean(abs(slitab$linear)) /2
slitab$strongly_category <- cut(slitab$linear, c(-Inf, -0.10, 0.10, Inf), 
                                labels = c("agree", "similar", "strongly"))
slitab$agree_category <- cut(slitab$linear, c(-Inf, -0.04, 0.04, Inf), 
                             labels = c("disagree", "neutral", "agree"))



# acquiescence
ccases$mean_agreement <- apply(ccases[,v$hexaco_items], 1, mean)




# ccases$unique_hexaco <- apply(ccases[,v$hexaco_items], 1, function(X) length(unique(X)))
# table(ccases$unique_hexaco, ccases$sample)
# ccases <- ccases[ccases$unique_hexaco == 5, ]


# residualised items and factors
v$resid_hexaco_items <- paste0("resid_", v$hexaco_items)
v$resid_hexaco_factors <- paste0("resid_", v$hexaco_factors)

ccases[,v$resid_hexaco_items] <- sapply(v$hexaco_items, function(X) 
    resid(regression(X, "hexaco_items_global_ml", ccases)))

# hexaco factors
meta.hexaco$residual <- paste0("resid_", meta.hexaco$id)
residfactors <- score_test(meta.hexaco, ccases, subscale_name = "factor", reverse="reversed", id = "residual")
rs <- data.frame(residfactors$scores[,v$hexaco_factors])
names(rs) <- v$resid_hexaco_factors
ccases[,names(rs)] <- rs

# make factor to control order of regression and so on
ccases$sample <- factor(ccases$sample, c("research", "applicant"))


acases <- ccases[ ccases$sample == "applicant", ]
ncases <- ccases[ ccases$sample == "research", ]


# It is convenient for the parallel analysis on the cluster if
# there are fewer dependencies on packages and s on.
# Thus, export key files for lavaan parallel analysis
saveRDS(ccases[,c("sample", v$hexaco_items, v$hexaco_big1, v$hexaco_facets, v$hexaco_factors)],
        file = "lavaan/lavaan-ccases.rds")
saveRDS(meta.hexaco, file = "lavaan/meta.hexaco.rds")
saveRDS(v, file = "lavaan/v.rds")

