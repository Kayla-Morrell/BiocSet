#' Functions to access reference urls for different identifiers
#'
#' @rdname url_ref
#'
#' @param es A BiocSet object in which the reference urls should be added to.
#'
#' @return For `url_ref_element()`, a BiocSet object with the url column added 
#'     to the element tibble.
#'
#' @export
#'
#' @examples
#' es <- BiocSet("GO:0000002" = c("TP53", "TNF"), "GO:0000003" = c("IL6"))
#' url_ref_element(es) 
url_ref_element <- function(es) 
{
    elements <- es_element(es)$element
    
    url <- ifelse(startsWith(elements, "ENSG"),
        "https://www.ensembl.org/Homo_sapiens/Gene/Summary?db=core;g=",
        "https://www.ncbi.nlm.nih.gov/gene/?term="
    )

    url <- paste0(url, elements)
    es %>% mutate_element(url = url)
}

#' @rdname url_ref
#' 
#' @return For `url_ref_set()`, a BiocSet object with the url column added to 
#'     the set tibble.
#' 
#' @export
#'
#' @examples
#'
#' url_ref_set(es)
url_ref_set <- function(es)
{
    sets <- as.character(es_set(es)$set)    

    url <- ifelse(startsWith(sets, "GO"),
        "http://amigo.geneontology.org/amigo/medial_search?q=",
        "https://www.genome.jp/dbget-bin/www_bget?pathway:"
    )

    url <- paste0(url, sets)
    es %>% mutate_set(url = url)
}

#' @rdname url_ref
#'
#' @return For `url_ref()`, a BiocSet object with the url column added to both 
#'     the element and set tibbles.
#'
#' @export
#'
#' @examples
#'
#' url_ref(es)
url_ref <- function(es)
{
    es %>% url_ref_element() %>% url_ref_set()
}
