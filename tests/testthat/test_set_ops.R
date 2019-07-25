context("set_ops")

test_that("'union.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    tbl <- union(es1, es2)
    expect_identical(dim(es_element(tbl)), c(12L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(12L, 2L))
})

test_that("'union_single.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])

    tbl <- union_single(es1)
    expect_identical(dim(es_element(tbl)), c(20L, 1L))
    expect_identical(dim(es_set(tbl)), c(1L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(20L, 2L))
})

test_that("'intersect.BiocSet()' works", 
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    tbl <- intersect(es1, es2)
    expect_identical(dim(es_element(tbl)), c(4L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(4L, 2L))

})

test_that("'intersect_single.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])

    tbl <- intersect_single(es1)
    expect_identical(dim(es_element(tbl)), c(7L, 1L))
    expect_identical(dim(es_set(tbl)), c(1L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(7L, 2L)) 
})
