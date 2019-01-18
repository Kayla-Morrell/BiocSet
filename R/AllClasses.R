setOldClass("tbl_gene")

setOldClass("tbl_set")

setOldClass("tbl_geneset")

.GeneSet <- setClass(
    "GeneSet",
    slots = c(
        gene = "tbl_gene",
        set = "tbl_set",
        geneset = "tbl_geneset"
    )
)
