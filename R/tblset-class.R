#' Set representation as an S3 class tibble
#' @rdname tblset
#'
#' @param tbl_geneset An S3 'geneset' tibble.
#'
#' @return An S3 'set' object in a tibble representation.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr distinct '%>%'
#'
#' @export
#'
#' @examples
#' tbl <- tbl_geneset(set1 = letters, set2 = LETTERS)
#' tbl_set(tbl)

tbl_set <-
    function(tbl_geneset)
{
    stopifnot(
        is_tbl_geneset(tbl_geneset)
    )

    tbl <- tibble(set = tbl_geneset$set) %>% distinct()
    subclass_tbl_geneset_base(tbl, "tbl_set")
}
