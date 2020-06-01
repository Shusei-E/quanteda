#' Experimental function for parallel tokenization
#' @param x a character vector
#' @param block_size number of documents in a block (only for development)
#' @param ... passed to tokens.tokens()
#' @export
#' @importFrom future.apply future_lapply
tokens_parallel <- function(x, block_size = 1000, ...) {
    
    if (!is.character(x))
        stop("x must be a character")
        
    temp <- split(x, ceiling(seq_along(x) / block_size))
    time <- proc.time()
    temp <- future_lapply(temp, function(y) {
        y <- preserve_special(y, split_hyphens = FALSE, split_tags = FALSE, verbose = FALSE)
        special <- attr(y, "special")
        y <- tokenize_word(y)
        y <- serialize_tokens(y)
        y <- restore_special(y, special, recompile = FALSE)
        y <- unclass(y)
        return(y)
    })
    cat("tokenizing... ", format((proc.time() - time)[3], digits = 3), "sec\n")
    type <- unique(unlist(lapply(temp, attr, "types"), use.names = FALSE))
    result <- lapply(temp, function(y) {
        map <- c(0L, fastmatch::fmatch(attr(y, "types"), type))
        y <- lapply(y, function(z) map[z + 1L])
        return(y)
    })
    cat("remapping... ", format((proc.time() - time)[3], digits = 3), "sec\n")
    result <- build_tokens(
        unlist(result, recursive = FALSE), 
        types = type,
        what = "word", 
        docvars = make_docvars(length(x))
    )
    cat("building... ", format((proc.time() - time)[3], digits = 3), "sec\n")
    result <- tokens.tokens(result, ...)
    return(result)
}