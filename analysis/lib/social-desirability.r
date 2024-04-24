
modal_distance <- function (data, modedata) {
    modes <- sapply(modedata, modal_response)
    meandist <- apply(sapply(names(modes), function(X) abs(data[,X] - modes[X])),
          1, mean)
    meandist
}

weighted_desirability <- function(data, meta) {
    items <- names(data)
    weighted_names <- paste0("weighted_", items)
    meta <- as.matrix(meta)
    for(i in seq(items)) {
        data[,weighted_names[i]] <- meta[items[i], data[,items[i]]]
    }
    apply(data[,weighted_names], 1, mean)
}



option_desirability <- function(combined_data = ccases,
                                applicant_data = acases, 
                                nonapplicant_data = ncases,
                                item, 
                                overall_desirability = "hexaco_items_global_ml", 
                                response_options = 1:5,
                                minimum_sample = 50) {
    # get proportion agree
    responses <- data.frame(response = response_options,
        prop_applicant =  sapply(response_options, 
                                function(X) mean(applicant_data[,item] == X)),
        prop_nonapplicant =  sapply(response_options, 
                                function(X) mean(nonapplicant_data[,item] == X)),
        combined_count = sapply(response_options, 
                               function(X) sum(applicant_data[,item] == X)),
        loading_raw = sapply(response_options, 
                            function(X) cor(combined_data[,item] == X, 
                                            combined_data[,overall_desirability]))
        )
    
    responses$prop_diff <- responses$prop_applicant - responses$prop_nonapplicant
    
    responses$centered_prop_applicant <- responses$prop_applicant -
        1/length(response_options)
    
    responses$loading_adjusted <- ifelse(responses$combined_count < minimum_sample,
                                         NA, responses$loading_raw)
    responses$loading_adjusted <- ifelse(is.na(responses$loading_adjusted),
                min(responses$loading_adjusted, na.rm = TRUE) - 0.05,
                responses$loading_adjusted)
                                         
    responses$desirability <- responses$centered_prop_applicant +
        responses$loading_adjusted
                      
    responses
}


items_option_desirability <- function(items = v$hexaco_items,
                                        combined_data = ccases,
                                        applicant_data = ccases[ ccases$sample == "applicant",], 
                                        nonapplicant_data = ccases[ ccases$sample == "research",],
                                        seed_desirability = "hexaco_items_global_ml", 
                                        response_options = 1:5,
                                        minimum_sample = 50){ 
    
    sdtabs <- lapply(items, function(X) 
        option_desirability(combined_data = combined_data,
                            applicant_data = applicant_data,
                            nonapplicant_data = nonapplicant_data,
                            item = X,
                            response_options = response_options,
                            minimum_sample = minimum_sample,
                            overall_desirability = seed_desirability
                            ))
    destab <- data.frame(t(sapply(sdtabs, function(X) X$desirability)))
    row.names(destab) <- items
    combined_data$updated_desirability <- weighted_desirability(
        combined_data[,items], destab)
    
    list(destab = destab, sdtabs = sdtabs,
         weighted_desirability = combined_data$updated_desirability)
    
}