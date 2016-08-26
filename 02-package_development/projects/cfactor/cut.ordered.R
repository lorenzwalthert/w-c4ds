cut.ordered <- function(x, breaks, labels = NULL, include.lowest = FALSE, 
                        right = TRUE, digit.lab = 3, ordered_result = FALSE, ...) {
        xnum <- as.numeric(x)
        b1 <- sapply(breaks, function(point) which(point == levels(x)))
        cut(xnum, breaks = c(min(xnum)-1, b1), labels = breaks, include.lowest = include.lowest, 
            right = right, digit.lab = digit.lab, ordered_result = ordered_result, ...)
                     
}