pcloading <- function(data) {
    - psych::principal(data)$loadings[,1]
}


items <- data.frame(id = meta.hexaco$id)
items$research_loading <- pcloading(ccases[ccases$sample == "research",items$id])
items$applicant_loading <- pcloading(ccases[ccases$sample == "applicant",items$id])

scases <- split(ccases[,items$id], ccases$sample)
items$research_mean <- sapply(scases$research, mean)
items$applicant_mean <- sapply(scases$applicant, mean)
items$researchdiff3_mean <- sapply(scases$research, function(X) abs(3 - mean(X)))
items$applicantdiff3_mean <- sapply(scases$applicant, function(X) abs(3 - mean(X)))
items$research_relativesd <- sapply(scases$research, function(X) relativeSD(X, 1, 5))
items$applicant_relativesd <- sapply(scases$applicant, function(X) relativeSD(X, 1, 5))
items$mean_diff <- sapply(scases$applicant, mean) - sapply(scases$research, mean)
items$pooled_sd <- sqrt((sapply(scases$applicant, var) + sapply(scases$research, var)) / 2)
items$d_diff <- items$mean_diff / items$pooled_sd
items$prop_diff <- items$mean_diff / (2 - abs(items$research_mean - 3))
items$sdratio <- sapply(scases$applicant, sd) / sapply(scases$research, sd)
items$relativesdratio <- items$applicant_relativesd / items$research_relativesd
items$abs_d_diff <- abs(items$d_diff)
items$abs_prop_diff <- abs(items$prop_diff)
items$abs_research_loading <- abs(items$research_loading)
items$abs_applicant_loading <- abs(items$applicant_loading)


# MacCann et al data
items <- merge(items, macitems, all.x = TRUE)
items$maccann_mean_diff <- items$maccann_fake_mean - items$maccann_honest_mean
items$maccann_pooled_sd <- sqrt((items$maccann_fake_mean^2 + items$maccann_honest_sd^2) / 2)
items$maccann_d_diff <- items$maccann_mean_diff / items$maccann_pooled_sd
items$inhexaco100 <- !is.na(items$hexaco100)


# MacCann et al absolute measures
items$maccann_abs_honest_global <- abs(items$maccann_honest_global)
items$maccann_abs_fake_global <- abs(items$maccann_fake_global)
items$maccann_honest_diff3_mean <- abs(items$maccann_honest_mean - 3)
items$maccann_fake_diff3_mean <- abs(items$maccann_fake_mean - 3)
items$maccann_abs_d_diff <- abs(items$maccann_d_diff)


v$directed_indices <- c("research_loading", "applicant_loading", 
                        "research_mean", "applicant_mean",
                        "d_diff",
                        "maccann_honest_global", "maccann_fake_global",
                        "maccann_honest_mean", "maccann_fake_mean",
                        "maccann_d_diff")
v$absolute_indices <- c( "abs_research_loading", "abs_applicant_loading",
                         "researchdiff3_mean", "applicantdiff3_mean",
                         "abs_d_diff", 
                         "maccann_abs_honest_global", "maccann_abs_fake_global", 
                         "maccann_honest_diff3_mean", "maccann_fake_diff3_mean",
                         "maccann_abs_d_diff")

# merge in factor and facet
items <- merge(items, meta.hexaco[,c("id", "reversed", "factor", "facet")])
items$gfp_factorreversal <- ifelse(items$factor != "emotionality", 1, -1)


