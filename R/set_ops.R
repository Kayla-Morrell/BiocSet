union.BiocSet <- function(x, y, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(x)
    tbl
}
