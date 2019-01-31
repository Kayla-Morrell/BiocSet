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

## FIXME: select.tbl_geneset_base mutate.tbl_geneset_base
## group_by.tbl_geneset_base ungroup.tbl_geneset_base
## arrange.tbl_geneset_base group_vars.tbl_geneset_base
## tbl_vars.tbl_geneset_base

## FIXME: summarise.tbl_geneset_base
## summarize.tbl_geneset_base <- summarise.tbl_geneset_base
