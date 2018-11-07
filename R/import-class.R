#' Importing and formating of gene sets as an S3 class tibble
#' @rdname import
#'
#' @param path For `import.gmt()`, a file name or URL containing gene sets.
#'
#' @importFrom rtracklayer import export resource
#' @importFrom methods new
#' @importFrom utils write.table
#'
#' @export
#'
#' @examples
#' gmtFile <- system.file(package = "GeneSet", "extdata",
#'     "hallmark.gene.symbol.gmt")
#' import.gmt(gmtFile)

import.gmt <- function(path) {
    sets <- readLines(path)
    sets <- strsplit(sets, "\t")
    names <- vapply(sets, function(set) set[[1]], character(1))
    genes <- lapply(sets, function(set) set[-(1:2)])
    names(genes) <- names

    source <- vapply(sets, function(set) set[[2]], character(1))
    source[source=="NA" | !nzchar(source)] <- NA
    tbl <- do.call(tbl_geneset, genes)
    tbl$source <- rep(source, lengths(genes))
    tbl
}
 
#' @rdname import
#' 
#' @export
.GMTFile = setClass("GMTFile", contains = "RTLFile")

#' @rdname import
#' 
#' @export
GMTFile = function(resource, ...)
    .GMTFile(resource = resource)

#' @rdname import
#'
#' @export
setMethod(
    "import", c("GMTFile", "ANY", "ANY"),
    function(con, format, text, ...)
{        
    import.gmt(resource(con))
})

#' @rdname import
#'
#' @param tbl For `export.tbl_geneset()`, a tbl_geneset that
#'     should be exported to a gmt file.
#'
#' @export

export.tbl_geneset <- function(tbl, path = tempfile(fileext = ".gmt")) {
    stopifnot(is_tbl_geneset(tbl))

    if(!"source" %in% names(tbl))
        tbl <- mutate(tbl, source = rep(NA_character_, nrow(tbl)))

    sets <- group_by(tbl, set) %>%
        summarise(source = unique(source),
                  gene = paste(gene, collapse = "\t"))


    write.table(sets, path, sep = "\t",
                col.names = FALSE, row.names = FALSE, quote = FALSE)
}

setOldClass("tbl_geneset")

#' @rdname import
#'
#' @export
setMethod(
    "export", c("tbl_geneset", "GMTFile", "ANY"),
    function(object, con, format, ...)
{
    export.tbl_geneset(object, resource(con))
})
