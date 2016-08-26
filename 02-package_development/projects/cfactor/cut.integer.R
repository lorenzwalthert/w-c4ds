cut.integer <- function(x, breaks, labels = NULL, include.lowest = FALSE, 
                        right = TRUE, digit.lab = 3, ordered_result = FALSE, ...) {
        b1 <- breaks
        b2 <- breaks - 1
        if(right == T) {
                shift <- 1
        } else {
                shift <- 0
        }
        lab <- sapply(seq_len(length(b1)-1), function(i) paste0(b1[i]+shift, " - ", b2[i + 1]+shift, sep = ""))
        
        cut.default(x, breaks = breaks, labels = lab, include.lowest = include.lowest, 
                    right = right, digit.lab = digit.lab, ordered_result = ordered_result, ...)
        
}