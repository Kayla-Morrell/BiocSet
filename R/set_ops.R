union.BiocSet <- function(x, y, ...)
{
    sub_x <- .active_value(x)
    sub_y <- .active_value(y)
    tbl <- union(sub_x, sub_y, ...)
    tbl
}
