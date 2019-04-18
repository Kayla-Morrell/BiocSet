#' Element representation as an S3 class tibble
#' @rdname tblelement
#'
#' @param tbl_elementset An S3 'elementset' tibble.
#'
#' @return An S3 'element' object in a tibble representation.
#'
#' @importFrom tibble tibble
#' @importFrom dplyr distinct '%>%'
#'
#' @export
#'
#' @examples
#' tbl <- tbl_elementset(set1 = letters, set2 = LETTERS)
#' tbl_element(tbl)

tbl_element <-
    function(tbl_elementset)
{
    stopifnot(
        is_tbl_elementset(tbl_elementset)
    )
    
    tbl <- tibble(element = tbl_elementset$element) %>% distinct()
    subclass_tbl_elementset_base(tbl, "tbl_element")
}
