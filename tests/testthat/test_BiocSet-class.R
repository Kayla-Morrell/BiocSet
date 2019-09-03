context("BiocSet-class")

test_that("'BiocSet()' works",
{
    es <- BiocSet(a = letters, b = LETTERS)
    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_element(es)), c(52L,1L))
    expect_identical(dim(es_set(es)), c(2L,1L))
    expect_identical(dim(es_elementset(es)), c(52L,2L))
    expect_true(is_tbl_elementset(es_elementset(es)))
    expect_identical(.active(es), "elementset")
    expect_length(.active(es), 1L)

    es <- BiocSet()
    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_elementset(es)), c(0L,2L))
    expect_true(is_tbl_elementset(es_elementset(es)))

    es <- BiocSet(set1 = character(), set2 = LETTERS)
    expect_s4_class(es, "BiocSet")
    expect_identical(dim(es_elementset(es)), c(26L,2L))
    expect_true(is_tbl_elementset(es_elementset(es)))
    expect_identical(es_elementset(es)$set, rep("set2", 26))
    
    expect_error(BiocSet(set1 = 1:10, set2 = LETTERS))
    expect_error(BiocSet(set1 = 1:10))
    expect_error(BiocSet(LETTERS))
    expect_error(BiocSet(set1 = letters, LETTERS))
    expect_error(BiocSet(set1 = letters, set2 = 1:10))
    expect_error(BiocSet(1:10))
})

test_that("'BiocSet_from_elementset()' works", {
    set.seed(123)
    element <- tibble(element = letters[1:10], v1 = sample(10), v2 = sample(10))
    set <- tibble(set = LETTERS[1:2], v1 = sample(2), v2 = sample(2))
    elementset <- 
        tibble(
            element = letters[1:10], 
            set = sample(LETTERS[1:2], 10, TRUE)
        )

    es <- BiocSet_from_elementset(elementset, element, set)
    expect_true(is_tbl_elementset(es_elementset(es)))
    expect_identical(dim(es_element(es)), c(10L, 3L))
    expect_identical(dim(es_set(es)), c(2L, 3L))
    expect_identical(dim(es_elementset(es)), c(10L, 2L))

    es1 <- BiocSet_from_elementset(elementset)
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_element(es1)), c(10L, 1L))
    expect_identical(dim(es_set(es1)), c(2L, 1L))
    expect_identical(dim(es_elementset(es1)), c(10L, 2L))

    es2 <- BiocSet_from_elementset(elementset, element)
    expect_true(is_tbl_elementset(es_elementset(es2)))
    expect_identical(dim(es_element(es2)), c(10L, 3L))
    expect_identical(dim(es_set(es2)), c(2L, 1L))
    expect_identical(dim(es_elementset(es2)), c(10L, 2L))

    es3 <- BiocSet_from_elementset(elementset, set = set)
    expect_true(is_tbl_elementset(es_elementset(es3)))
    expect_identical(dim(es_element(es3)), c(10L, 1L))
    expect_identical(dim(es_set(es3)), c(2L, 3L))
    expect_identical(dim(es_elementset(es3)), c(10L, 2L))

    expect_error(BiocSet_from_elementset())
    expect_error(BiocSet_from_elementset(elementset = 1:5))
})
