context("elementset_funs")

test_that("'tibble_from_element()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% tibble_from_element()
    expect_s3_class(es1, "tbl_df")
    expect_identical(dim(es1), c(52L, 2L))
    expect_true(.is_tbl_elementset(es1))
})

test_that("'data.frame_from_elementset()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% data.frame_from_elementset()
    expect_identical(class(es1), "data.frame")
    expect_identical(dim(es1), c(52L, 2L))
    expect_true(.is_tbl_elementset(es1))
})
