.element <- function(x) x@element

.set <- function(x) x@set

.elementset <- function(x) x@elementset

.active <- function(x) x@active

`.active<-` <- function(x, value)
{
    value <- gsub('"', '', value)
    stopifnot(value %in% c("element", "set", "elementset"))
    x@active <- value
    x
}

#' @importFrom methods slot
.active_value <-
    function(x)
{
    slot(x, .active(x))
}

.test <-
    function(x)
{
    all(lengths(x) == 1L) &&
        length(unique(sapply(x, typeof))) == 1L
}
