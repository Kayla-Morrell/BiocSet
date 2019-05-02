.subclasses <- c("tbl_elementset_base",
                "tbl_element",
                "tbl_set",
                "tbl_elementset")

subclass_tbl_elementset_base <-
    function(x, subclass)
{
    class(x) <- c(subclass, "tbl_elementset_base", class(x))
    x
}

#' @importFrom dplyr filter
#'
#' @export
filter.tbl_elementset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr select
#'
#' @export
select.tbl_elementset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr mutate
#'
#' @export
mutate.tbl_elementset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr tbl_vars
#'
#' @export
tbl_vars.tbl_elementset_base <-
    function(x)
{
    class = class(x)
    class(x) = setdiff(class, .subclasses)
    tbl_vars(x)
}

## #' @importFrom dplyr ungroup
## #'
## #' @export
## ungroup.tbl_elementset_base <-
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
summarise.tbl_elementset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

summarize.tbl_elementset_base <- summarise.tbl_elementset_base

#' @importFrom dplyr arrange
#'
#' @export
arrange.tbl_elementset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr union
#'
#' @export
union.tbl_elementset_base <- function(x, y, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(x)
    tbl
}

#' @importFrom dplyr intersect
#' 
#' @export
intersect.tbl_elementset_base <- function(x, y, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(x)
    tbl
}
