context("tblgeneset-class")

test_that("'tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, B = LETTERS)
    expect_s3_class(gs, "tbl_geneset")
})
