#' Functions to access help for the different identifiers
#'
#' @rdname url_ref
#'
#' @param es A BiocSet object in which the reference urls should be added to.
#'
#' @return A BiocSet object with the url column added to element or set.
#'
#' @export
#'
#' @examples
#' es <- BiocSet(set1 = c("TP53", "TNF", "EGFR"), set2 = c("IL6", "VEGFA"))
#' url_ref_element(es)
url_ref_element <- function(es) 
{
    elements <- es_element(es)$element
    # rework this code...get rid of vapply and just create a vector with 
    # 'startsWith'. That why 'startsWith' and 'paste0' are only called once 
    # for all entries instead of for each entry
    url <- vapply(elements, function(x) {
        if (startsWith(x, "ENSG")) {
            paste0(
                "https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=",
                x
            )
        }
        else
            paste0(
                "https://www.ncbi.nlm.nih.gov/gene/?term=",
                x
            )
    }, character(1))

    es %>% mutate_element(url = url)
}

#' @rdname url_ref
#' 
#' @export
#'
#' @examples
#' es <- BiocSet("GO:0000002" = c("TP53", "TNF"), "GO:0000003" = c("IL6")) 
#' url_ref_set(es)
url_ref_set <- function(es)
{
    sets <- as.character(es_set(es)$set)
    # rework this code...get rid of vapply and just create a vector with
    # 'startsWith'. That why 'startsWith' and 'paste0' are only called once
    # for all entries instead of for each entry
    url <- vapply(sets, function(x) {
        if (startsWith(x, "GO")) {
            paste0(
                "http://amigo.geneontology.org/amigo/medial_search?q=",
                x
            )
        }
        else
            paste0(
                "https://www.genome.jp/dbget-bin/www_bget?pathway:",
                x
            )
        }, character(1))

    es %>% mutate_set(url = url)
}

# make url_ref() that does both url_ref_element() and url_ref_set()
