cfactor <- function(x, levels, labels = levels, ...) {
        
        `%w/o%` <- function(x, y) x[!x %in% y]
        
        prior <- as.character(unique(x))
        output<-factor(x, levels, labels, ...)
        if(!setequal(prior, labels)) {
                # labels that are not current names
                if(!all(labels %in% prior)) {
                        warning(paste("the following label(s) is / are empty:", c(labels %w/o% prior)),
                                call. = F)
                }
                
                # current names that don't become labels
                if(!all(prior %in% labels)) {
                        warning(paste("the following label(s) is / are removed:", prior[!(prior %in% labels)]),
                                call. = F)
                }

                # all old labels in new labels?
                # all(labels %in% prior) # all new labels in old labels?
                
                
        }
        output
}