#' Importing and formating of gene sets as an S3 class tibble
#' @rdname import
#'
#' @export
.GMTFile = setClass("GMTFile", contains = "RTLFile")

#' @rdname import
#'
#' @param resource For `GMTFile()`, the .gmt file that will be imported in.
#'
#' @return For `GMTFile()`, an object representing the path to a .gmt file on disk
#'
#' @export
GMTFile = function(resource, ...)
    .GMTFile(resource = resource)

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
#' import(gmtFile)

import.gmt <- function(path) {
    sets <- readLines(path)
    sets <- strsplit(sets, "\t")
    names <- vapply(sets, function(set) set[[1]], character(1))
    genes <- lapply(sets, function(set) set[-(1:2)])
    names(genes) <- names

    source <- vapply(sets, function(set) set[[2]], character(1))
    source[source=="NA" | !nzchar(source)] <- NA
    tbl <- do.call(GeneSet, genes)
    tbl <- tbl %>% mutate(source = rep(source, lengths(genes)))
    tbl
}


#' @rdname import
#'
#' @param con For `import()`, the file name or URL the gene set is
#'     loaded from. For `export()`, the file name or URL the gene set
#'     is to be saved to.
#' @param format For `import()`, the format of the output.
#' @param text For `import()`, if con is missing this is a character
#'     vector directly providing the gene set that should be imported.
#' @param ... Parameters to pass to the format-specific method
#'
#' @return For `import()`, tbl_geneset
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
#' @param tbl For `export.GeneSet()`, a GeneSet that
#'     should be exported to a gmt file.
#'
#' @export

export.GeneSet <- function(tbl, path = tempfile(fileext = ".gmt")) {
    stopifnot(is_tbl_geneset(.geneset(tbl)))

    if(!"source" %in% names(.geneset(tbl)))
        tbl <- mutate(.geneset(tbl),
                            source = rep(NA_character_, nrow(.geneset(tbl))))

    sets <- group_by(tbl, set) %>%
        summarise(source = unique(source),
                  gene = paste(gene, collapse = "\t"))


    write.table(sets, path, sep = "\t",
                col.names = FALSE, row.names = FALSE, quote = FALSE)
}

#' @rdname import
#'
#' @param object For `export()`, the object to be exported.
#'
#' @return For `export()`, a GMTFile object representing the location
#'     where the gene set was written
#'
#' @export
setMethod(
    "export", c("GeneSet", "GMTFile", "ANY"),
    function(object, con, format, ...)
{
    export.GeneSet(object, resource(con))
    con
})
