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
    
    url <- ifelse(startsWith(elements, "ENSG"),
        paste0(
            "https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=",
            elements
            ),
        paste0(
            "https://www.ncbi.nlm.nih.gov/gene/?term=",
            elements
        )
    )

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
    
    url <- ifelse(startsWith(sets, "GO"),
        paste0(
            "http://amigo.geneontology.org/amigo/medial_search?q=",
            sets
            ),
        paste0(
            "https://www.genome.jp/dbget-bin/www_bget?pathway:",
            sets
            )
    )

    es %>% mutate_set(url = url)
}

#' @rdname url_ref
#'
#' @export
#'
#' @examples
#' es <- BiocSet("GO:0000002" = c("TP53", "TNF"), "GO:0000003" = c("IL6"))
#' url_ref(es)
url_ref <- function(es)
{
    es %>% url_ref_element() %>% url_ref_set()
}
