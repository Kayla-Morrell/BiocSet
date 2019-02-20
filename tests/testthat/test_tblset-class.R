context("tblset-class")

test_that("'tbl_set()' works", {
    gs <- tbl_geneset(set1 = letters, set2 = LETTERS)

    gs1 <- tbl_set(gs)
    expect_s3_class(gs1, "tbl_set")
    expect_identical(class(gs1), c("tbl_set", "tbl_geneset_base", "tbl_df",
                                   "tbl", "data.frame"))
    expect_length(class(gs1), 5)
    expect_identical(dim(gs1), c(2L,1L))
    expect_identical(levels(gs1$set), levels(gs$set))
    expect_false(is_tbl_geneset(gs1))
})
