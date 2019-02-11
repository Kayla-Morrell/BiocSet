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

#' @importFrom dplyr group_by
#'
#' @export
group_by.tbl_geneset_base <-
    function(.data, ..., add = FALSE)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

#' @importFrom dplyr ungroup
#'
#' @export
ungroup.tbl_geneset_base <-
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
}

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

#' @importFrom dplyr group_vars
#'
#' @export
group_vars.tbl_geneset_base <-
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
    function(.data, ...)
{
    tbl <- NextMethod()
    class(tbl) <- class(.data)
    tbl
} 
