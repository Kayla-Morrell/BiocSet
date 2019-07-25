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

#' @importFrom dplyr tbl_nongroup_vars
#'
#' @export
tbl_nongroup_vars.tbl_elementset_base <-
    function(x)
{
    class = class(x)
    class(x) = setdiff(class, .subclasses)
    tbl_nongroup_vars(x)
}

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
    stopifnot(is(x) == is(y))
    tbl <- NextMethod()
    class(tbl) <- class(x)
    tbl
}

#' @importFrom dplyr intersect
#' 
#' @export
intersect.tbl_elementset_base <- function(x, y, ...)
{
    stopifnot(is(x) == is(y))
    tbl <- NextMethod()
    class(tbl) <- class(x)
    tbl
}
