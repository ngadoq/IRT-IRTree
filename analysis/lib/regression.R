regression <- function(dv, ivs, data) {
    # run a linear model with text arguments for dv and ivs
    iv_string <- paste(ivs, collapse=" + ")
    regression_formula <- as.formula(paste(dv, iv_string, sep=" ~ "))
    lm(regression_formula, data)
    
}