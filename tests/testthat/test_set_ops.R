context("set_ops")

test_that("'union.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    expect_s4_class(es1, "BiocSet")
    expect_s4_class(es2, "BiocSet")

    tbl <- union(es1, es2)
    expect_s4_class(tbl, "BiocSet")
    expect_identical(dim(es_element(tbl)), c(12L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(12L, 2L))
    expect_true(is_tbl_elementset(es_elementset(tbl)))
})

test_that("'intersect.BiocSet()' works", {
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    expect_s4_class(es1, "BiocSet")
    expect_s4_class(es2, "BiocSet")

    tbl <- intersect(es1, es2)
    expect_s4_class(tbl, "BiocSet")
    expect_identical(dim(es_element(tbl)), c(4L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(4L, 2L))
    expect_true(is_tbl_elementset(es_elementset(tbl)))
})
