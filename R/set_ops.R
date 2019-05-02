#' Set operations for BiocSets
#'
#' @rdname set_op
#'
#' @param x The first BiocSet to perform operations on
#' @param y The second BiocSet to perform operations on
#' 
#' @return For union, a tibble with the results of the union of x and y
#' 
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
#' es2 <- BiocSet(set1 = letters[c(2:4)], set2 = LETTERS[c(2:4)])
#' union(es1, es2)
union.BiocSet <- function(x, y, ...)
{
    sub_x <- .active_value(x)
    sub_y <- .active_value(y)
    tbl <- union(sub_x, sub_y, ...)
    tbl
}

#' @rdname set_op
#'
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
#' es2 <- BiocSet(set1 = letters[c(2:4)], set2 = LETTERS[c(2:4)])
#' union_element(es1, es2)
union_element <- function(x, y, ...)
{
    sub_x <- es_activate(x, "element")
    sub_y <- es_activate(y, "element")
    union(sub_x, sub_y, ...)
}

#' @rdname set_op
#'
#' @export
#' 
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
#' es2 <- BiocSet(set1 = letters[c(2:4)], set2 = LETTERS[c(2:4)])
#' union_set(es1, es2)
union_set <- function(x, y, ...)
{
    sub_x <- es_activate(x, "set")
    sub_y <- es_activate(y, "set")
    union(sub_x, sub_y)
}

#' @rdname set_op
#'
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
#' es2 <- BiocSet(set1 = letters[c(2:4)], set2 = LETTERS[c(2:4)])
#' union_elementset(es1, es2)
union_elementset <- function(x, y, ...)
{
    sub_x <- es_activate(x, "elementset")
    sub_y <- es_activate(y, "elementset")
    union(sub_x, sub_y)
}

#' @rdname set_op
#'
#' 
#' @return For intersect, a tibble with the intersect of x and y 
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#' intersect(es1, es2)
intersect.BiocSet <- function(x, y, ...)
{
    sub_x <- .active_value(x)
    sub_y <- .active_value(y)
    tbl <- intersect(sub_x, sub_y, ...)
    tbl
}

#' @rdname set_op
#'
#' @export
#' 
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#' intersect_element(es1, es2)
intersect_element <- function(x, y, ...)
{
    sub_x <- es_activate(x, "element")
    sub_y <- es_activate(y, "element")
    intersect(sub_x, sub_y)
}

#' @rdname set_op
#'
#' @export
#' 
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#' intersect_set(es1, es2)
intersect_set <- function(x, y, ...)
{
    sub_x <- es_activate(x, "set")
    sub_y <- es_activate(y, "set")
    intersect(sub_x, sub_y)
}

#' @rdname set_op
#'
#' @export
#'
#' @examples
#' es1 <- BiocSet(set1 = letters[c(1:4)], set2 = LETTERS[c(1:4)])
#' es2 <- BiocSet(set1 = letters[c(3:8)], set2 = LETTERS[c(3:8)])
#' intersect_elementset(es1, es2)
intersect_elementset <- function(x, y, ...)
{
    sub_x <- es_activate(x, "elementset")
    sub_y <- es_activate(y, "elementset")
    intersect(sub_x, sub_y)
}
