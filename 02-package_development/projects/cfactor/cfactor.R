cfactor <- function(x, levels = NULL, labels = levels, connector = c("to", "bis", "-"),
                    cleaning = list(punct = c(from = ",'.", to = "."),
                                    connector = list(from = c("to", "-", "bis"), to = " - "),
                                    case = "upper",
                                    numbersAsStrings = F),
                    ...) {
        
        `%w/o%` <- function(x, y) x[!x %in% y]
        
        # detect factor levels
        if(is.null(levels)){
                connector.ready <- paste0(connector, collapse = "|")
                sep <- regexec(connector.ready, x)
                start <- sapply(sep, "[[", 1) # extract start 
                before <- substr(x, 1, start - 1)
                before <- gsub("[[:space:]]", "", before)
                finalorder <- order(as.numeric(gsub("[^[:digit:]]", "", before)))
                
                finallevels <- unique(x)[finalorder]
        } else {
                finallevels <- levels
        }
        
        #  clean levels
        if(!is.null(cleaning[["punct"]])) {
                regex <- paste("[", cleaning[["punct"]][["from"]], "]", sep = "")
        }
        
        
        # check for empty / removed
        prior <- as.character(unique(x))
        output<-factor(x, levels = finallevels, labels = finallevels, ...)
        if(!setequal(prior, levels(output))) {
                # levels that are not current names
                if(!all(levels(output) %in% prior)) {
                        warning(paste("the following label(s) is / are empty: \n", paste(c(levels(output) %w/o% prior), collapse = "\n")),
                                call. = F)
                }
                
                # current names that don't become levels
                if(!all(prior %in% levels(output))) {
                        warning(paste("the following label(s) is / are removed: \n", paste(prior[!(prior %in% levels(output))], collapse = "\n")),
                                call. = F)
                }
        }
        output
}