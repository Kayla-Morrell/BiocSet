context("intersect_single")

test_that("'intersect_single.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:10)], set2 = letters[c(4:20)])

    tbl <- intersect_single(es1)
    expect_identical(dim(es_element(tbl)), c(7L, 1L))
    expect_identical(dim(es_set(tbl)), c(1L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(7L, 2L)) 
})
