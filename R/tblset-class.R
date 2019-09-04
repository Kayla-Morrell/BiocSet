#' Set representation as an S3 class tibble
#' @rdname tblset
#' @name tblset
#' @param tbl_elementset An S3 \code{elementset} tibble.
#' @return An S3 \code{set} object in a tibble representation.
#' @importFrom tibble tibble
#' @importFrom dplyr distinct '%>%'
#' @keywords internal
#' @examples
#' tbl <- BiocSet:::.tbl_elementset(set1 = letters, set2 = LETTERS)
#' BiocSet:::.tbl_set(tbl)

.tbl_set <-
    function(tbl_elementset)
{
    stopifnot(
        .is_tbl_elementset(tbl_elementset)
    )

    tbl <- tibble(set = tbl_elementset$set) %>% distinct()
    subclass_tbl_elementset_base(tbl, "tbl_set")
}
