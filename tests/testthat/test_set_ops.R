context("set_ops")

test_that("'union_1arg()', works",
{
    es1 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])

    tbl <- union_1arg(es1)
    expect_s3_class(tbl, "tbl_element")
    expect_identical(dim(tbl), c(20L, 1L))
})

test_that("'union_2arg()', works",
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    tbl <- union_2arg(es1, es2)
    expect_s4_class(tbl, "BiocSet")
    expect_identical(dim(es_element(tbl)), c(12L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(12L, 2L))
    expect_true(is_tbl_elementset(es_elementset(tbl)))
})

test_that("'union.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    tbl <- union(es1, es2)
    expect_identical(dim(es_element(tbl)), c(12L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(12L, 2L))

    es3 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])

    tbl <- union(es3)
    expect_identical(dim(tbl), c(20L, 1L))
})

test_that("'intersect_1arg()' works",
{
    es1 <- BiocSet(set1 = letters[c(2:15)], set2 = letters[c(1:7)])

    tbl <- intersect_1arg(es1)
    expect_s3_class(tbl, "tbl_element")
    expect_identical(dim(tbl), c(6L, 1L))
})

test_that("'intersect_2arg()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = letters[c(10:15)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = letters[c(8:23)])

    tbl <- intersect_2arg(es1, es2)
    expect_s4_class(tbl, "BiocSet")
    expect_identical(dim(es_element(tbl)), c(8L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(8L, 2L))
    expect_true(is_tbl_elementset(es_elementset(tbl)))
})

test_that("'intersect.BiocSet()' works", 
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    tbl <- intersect(es1, es2)
    expect_identical(dim(es_element(tbl)), c(4L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(4L, 2L))

    es3 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])

    tbl <- intersect(es3)
    expect_identical(dim(tbl), c(7L, 1L))
})
