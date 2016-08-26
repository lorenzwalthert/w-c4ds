quantile.ordered <- function(x, probs = seq(0, 1, 0.25)) {
        # code adapted from https://gist.github.com/jrnold/6759254
        
        tab <- table(x)
        cdf <- cumsum(tab / sum(tab))
        idx <- sapply(probs, function(p) min(which(cdf >= p)))
        levels(x)[idx] 
}


