#' Functions to access help for the different identifiers
#'
#' @rdname url_ref
#'
#' @param search character, the id to look up. For KEGG, "map" will have to 
#'               replace the letters before the numbers, e.g. "hsa05310" should
#'               become "map05310".
#' @param ... additional arguments to pass to 'browseURL'.
#'
#' @importFrom utils browseURL
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#' url_ref("GO:0000002")
url_ref <- function(search, ...) 
{
    if (grepl("GO", search)) {
        browseURL(paste(
            "http://amigo.geneontology.org/amigo/medial_search?q=",
            search,
            "&searchtype=all",
            sep = ""
            ), ...)
    }
    else if (grepl("ENSG", search)) {
        browseURL(paste(
            "https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=",
            search,
            ";r=19:14091688-14118084",
            sep = ""
            ), ...)
    }
    else if (grepl("map", search)) {
        browseURL(paste(
            "https://www.genome.jp/dbget-bin/www_bget?pathway:",
            search,
            sep = ""
            ), ...)
    }
    else
        browseURL(paste(
            "https://www.ncbi.nlm.nih.gov/gene/?term=",
            search,
            sep = ""
            ), ...)
}
