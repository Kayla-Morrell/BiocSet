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

#' @importFrom dplyr group_by
#'
#' @export
group_by.tbl_geneset_base <-
    function(.data, ..., add = FALSE)
{
    tbl <- NextMethod()
    class = class(.data)
    idx = class %in% .subclasses
    class(tbl) <- c(class[idx], "grouped_df", class[!idx])
    tbl
}

#' @importFrom dplyr group_vars
#'
#' @export
group_vars.tbl_geneset_base <-
    function(.data, ...)
{
    class = class(.data)
    class(.data) = setdiff(class, .subclasses)
    group_vars(tmp)
}

#' @importFrom dplyr tbl_vars
#'
#' @export
tbl_vars.tbl_geneset_base <-
    function(.data, ...)
{
    class = class(.data)
    class(.data) = setdiff(class, .subclasses)
    tbl_vars(tmp)
}

#' @importFrom dplyr ungroup
#'
#' @export
ungroup.tbl_geneset_base <-
    function(.data, ...)
{
    class <- class(.data)
    class(.data) <- setdiff(class, .subclasses)
    tbl <- ungroup(.data, ...)
    class(tbl) <- setdiff(class, "grouped_df")
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
