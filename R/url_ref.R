#' Functions to access help for the different identifiers
#'
#' @rdname url_ref
#'
#' @param search character, the id to look up. For KEGG, "map" will have to 
#'     replace the letters before the numbers, e.g. "hsa05310" should
#'     become "map05310".
#' @param ... additional arguments to pass to 'browseURL'.
#'
#' @importFrom utils browseURL
#'
#' @return A default browser will open with the information about the id 
#'     displayed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' url_ref("GO:0000002")
#' }
# Thoughts about when calling url_ref() creating an extra column in element
# and set that have the url ready if the user wants to look it up
url_ref <- function(search, ...) 
{
    if (startsWith(search, "GO")) {
        url <- paste0(
            "http://amigo.geneontology.org/amigo/medial_search?q=",
            search,
            "&searchtype=all"
            )
    }
    else if (startsWith(search, "ENSG")) {
        url <- paste0(
            "https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=",
            search,
            ";r=19:14091688-14118084"
            )
    }
    else if (startsWith(search, "map")) {
        url <- paste0(
            "https://www.genome.jp/dbget-bin/www_bget?pathway:",
            search
            )
    }
    else
        url <- paste0(
            "https://www.ncbi.nlm.nih.gov/gene/?term=",
            search
            )

    browseURL(url, ...)
}
