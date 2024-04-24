# equate_gender <- function(x) {
#     # x is gender
#     gt <- table(x)
#     mincat <- names(which.min(gt))
#     mincount <- as.numeric(gt[mincat])
#     maxcat <- names(which.max(gt))
#     maxcount <- as.numeric(gt[maxcat])
#     samp <- sampling::strata(data.frame(sex = x), "sex", c(mincount, mincount),
#                              method = "srswor")
#     seq(x)  %in% samp$ID_unit
# }
# 
# 
# equate_age <- function(x) {
#     # x is age category
#     gt <- table(x)
#     mincat <- names(which.min(gt))
#     mincount <- as.numeric(gt[mincat])
#     maxcat <- names(which.max(gt))
#     maxcount <- as.numeric(gt[maxcat])
#     samp <- sampling::strata(data.frame(sex = x), "sex", c(mincount, mincount),
#                              method = "srswor")
#     seq(x)  %in% samp$ID_unit
# }
# 
# 
# debugonce(equate_age)
# equate_age(ecases$age_category)