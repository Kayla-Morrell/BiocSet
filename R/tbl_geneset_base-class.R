.subclasses <- c("tbl_geneset_base", "tbl_gene", "tbl_set", "tbl_geneset")

subclass_tbl_geneset_base <-
    function(x, subclass)
{
    class(x) <- c(subclass, "tbl_geneset_base", class(x))
    x
}

#' @importFrom dplyr filter
#'
#' @export
filter.tbl_geneset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr select
#'
#' @export
select.tbl_geneset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr mutate
#'
#' @export
mutate.tbl_geneset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr tbl_vars
#'
#' @export
tbl_vars.tbl_geneset_base <-
    function(x)
{
    class = class(x)
    class(x) = setdiff(class, .subclasses)
    tbl_vars(x)
}

## #' @importFrom dplyr ungroup
## #'
## #' @export
## ungroup.tbl_geneset_base <-
##     function(x, ...)
## {
##     class <- class(x)
##     class(x) <- setdiff(class, .subclasses)
##     tbl <- ungroup(x, ...)
##     class(tbl) <- setdiff(class, "grouped_df")
##     tbl
## }

#' @importFrom dplyr summarise
#'
#' @export
summarise.tbl_geneset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

summarize.tbl_geneset_base <- summarise.tbl_geneset_base

#' @importFrom dplyr arrange
#'
#' @export
arrange.tbl_geneset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}
