#' Gene representation as an S3 class tibble
#' @rdname tblgene
#'
#' @param tbl_geneset An S3 'geneset' tibble.
#'
#' @return An S3 'gene' object in a tibble representation.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr distinct '%>%'
#'
#' @export
#'
#' @examples
#' tbl <- tbl_geneset(set1 = letters, set2 = LETTERS)
#' tbl_gene(tbl)

tbl_gene <-
    function(tbl_geneset)
{
    stopifnot(
        is_tbl_geneset(tbl_geneset)
    )
    
    tbl <- tibble(gene = tbl_geneset$gene) %>% distinct()
    subclass_tbl_geneset_base(tbl, "tbl_gene")
}
