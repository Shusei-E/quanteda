
subset_fcm <- function(x, i, j, ..., drop) {
    
    attrs <- attributes(x)
    error <- FALSE
    if (!missing(i)) {
        if (is.character(i) && any(!i %in% rownames(x))) error <- TRUE
        if (is.numeric(i) && any(i > nrow(x))) error <- TRUE
    }
    if (!missing(j)) {
        if (is.character(j) && any(!j %in% colnames(x))) error <- TRUE
        if (is.numeric(j) && any(j > ncol(x))) error <- TRUE
    }
    if (error) stop("Subscript out of bounds")
    
    if (missing(i) && missing(j)) {
        return(x)
    } else if (!missing(i) && missing(j)) {
        x <- "["(as(x, "Matrix"), i, , ..., drop = FALSE)
    } else if (missing(i) && !missing(j)) {
        x <- "["(as(x, "Matrix"), , j, ..., drop = FALSE)
    } else {
        x <- "["(as(x, "Matrix"), i, j, ..., drop = FALSE)    
    }
    matrix2fcm(x, attrs)
}

#' @param i index for features
#' @param j index for features
#' @param drop always set to \code{FALSE}
#' @param ... additional arguments not used here
#' @rdname fcm-class
#' @export
#' @examples 
#' # fcm subsetting
#' y <- fcm(tokens(c("this contains lots of stopwords",
#'                   "no if, and, or but about it: lots"),
#'                 remove_punct = TRUE))
#' y[1:3, ]
#' y[4:5, 1:5]
#' 
#' 
setMethod("[", signature = c("fcm", i = "index", j = "index", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "index", j = "index", drop = "logical"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "missing", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "missing", drop = "logical"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "index", j = "missing", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "index", j = "missing", drop = "logical"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "index", drop = "missing"), subset_fcm)

#' @rdname fcm-class
#' @export
setMethod("[", signature = c("fcm", i = "missing", j = "index", drop = "logical"), subset_fcm)