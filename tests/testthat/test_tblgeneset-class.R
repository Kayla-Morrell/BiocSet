context("tblgeneset-class")

test_that("'tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, B = LETTERS)
    expect_s3_class(gs, "tbl_geneset")
    expect_identical(dim(gs), c(52L,2L))
    expect_true(is_tbl_geneset(gs))

    gs <- tbl_geneset()
    expect_s3_class(gs, "tbl_geneset")
    expect_identical(dim(gs), c(0L,2L))
    expect_true(is_tbl_geneset(gs))

    gs <- tbl_geneset(a = character(), b = LETTERS)
    expect_s3_class(gs, "tbl_geneset")
    expect_identical(dim(gs), c(26L,2L))
    expect_true(is_tbl_geneset(gs))
    expect_identical(levels(gs$set), c("a", "b"))

    expect_error(tbl_geneset(a = 1:5, B = LETTERS))
    expect_error(tbl_geneset(a = 1:5))
    expect_error(tbl_geneset(letters))
    expect_error(tbl_geneset(a = letters, LETTERS))
})

test_that("'is_tbl_geneset()' works", {
    expect_true(is_tbl_geneset(tibble(gene = character(), set = factor())))
    expect_true(is_tbl_geneset(tibble(gene = character(), set = factor(), x = integer())))
    expect_false(is_tbl_geneset(tibble(gene = integer(), set = factor())))
    expect_false(is_tbl_geneset(tibble(gene = character(), set = integer())))
    expect_false(is_tbl_geneset(tibble()))
    expect_false(is_tbl_geneset(tibble(gene = character())))
})
