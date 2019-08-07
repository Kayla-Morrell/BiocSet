context("tblelementset-class")

test_that("'tbl_elementset()' works", {
    es <- tbl_elementset(a = letters, B = LETTERS)
    expect_s3_class(es, "tbl_elementset")
    expect_identical(dim(es), c(52L,2L))
    expect_true(is_tbl_elementset(es))

    es <- tbl_elementset()
    expect_s3_class(es, "tbl_elementset")
    expect_identical(dim(es), c(0L,2L))
    expect_true(is_tbl_elementset(es))

    es <- tbl_elementset(a = character(), b = LETTERS)
    expect_s3_class(es, "tbl_elementset")
    expect_identical(dim(es), c(26L,2L))
    expect_true(is_tbl_elementset(es))
    expect_identical(es$set, rep("b", 26))

    expect_error(tbl_elementset(a = 1:5, B = LETTERS))
    expect_error(tbl_elementset(a = 1:5))
    expect_error(tbl_elementset(letters))
    expect_error(tbl_elementset(a = letters, LETTERS))
})

test_that("'is_tbl_elementset()' works", {
    expect_true(is_tbl_elementset(tibble(element = character(),
                                         set = character())))
    expect_true(is_tbl_elementset(tibble(element = character(),
                                         set = character(),
                                         x = integer())))
    expect_false(is_tbl_elementset(tibble(element = integer(),
                                          set = factor())))
    expect_false(is_tbl_elementset(tibble(element = character(),
                                          set = integer())))
    expect_false(is_tbl_elementset(tibble()))
    expect_false(is_tbl_elementset(tibble(element = character())))
})
