#' Experimental function for parallel tokenization
#' @param x a character vector
#' @param block_size number of documents in a block (only for development)
#' @param lapply family function for itteration
#' @param ... passed to tokens.tokens()
#' @export
#' @importFrom future.apply future_lapply
tokens_parallel <- function(x, docnames = NULL, block_size = 10000, FUN = lapply, ...) {
    
    if (!is.character(x))
        stop("x must be a character")
    cat(deparse(substitute(FUN)), "\n")
    if (!is.null(docnames))
        names(x) <- docnames
    x <- split(x, ceiling(seq_along(x) / block_size))
    time <- proc.time()
    cat("tokenizing...\n")
    x <- FUN(x, function(y) {
        cat("   ", head(names(y), 1), "to", tail(names(y), 1), "\n")
        y <- preserve_special(y, split_hyphens = FALSE, split_tags = FALSE, verbose = FALSE)
        special <- attr(y, "special")
        y <- tokenize_word(y, verbose = FALSE)
        y <- serialize_tokens(y)
        y <- restore_special(y, special, recompile = FALSE)
        y <- unclass(y)
        return(y)
    })
    cat("tokenizing... ", format((proc.time() - time)[3], digits = 3), "sec\n")
    type <- unique(unlist(lapply(x, attr, "types"), use.names = FALSE))
    result <- lapply(x, function(y) {
        map <- c(0L, fastmatch::fmatch(attr(y, "types"), type))
        y <- lapply(y, function(z) map[z + 1L])
        return(y)
    })
    cat("remapping... ", format((proc.time() - time)[3], digits = 3), "sec\n")
    result <- build_tokens(
        unlist(result, recursive = FALSE), 
        types = type,
        what = "word", 
        docvars = make_docvars(length(x), docnames)
    )
    cat("building... ", format((proc.time() - time)[3], digits = 3), "sec\n")
    result <- tokens.tokens(result, ...)
    return(result)
}

