context("tblgene-class")

test_that("'tbl_gene()' works", {
    gs <- tbl_geneset(set1 = letters, set2 = LETTERS)

    gs1 <- tbl_gene(gs)
    expect_s3_class(gs1, "tbl_gene")
    expect_identical(class(gs1), c("tbl_gene", "tbl_geneset_base", "tbl_df",
                                   "tbl", "data.frame"))
    expect_length(class(gs1), 5)
    expect_identical(gs1$gene, gs$gene)
    expect_identical(dim(gs1), c(52L,1L))
    expect_false(is_tbl_geneset(gs1))
})
