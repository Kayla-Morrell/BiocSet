context("set_funs")

test_that("'tibble_from_set()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% tibble_from_set()
    expect_s3_class(es1, "tbl_df")
    expect_identical(dim(es1), c(2L, 2L))
    expect_false(.is_tbl_elementset(es1))
})

test_that("'data.frame_from_set()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)
    
    es1 <- es %>% data.frame_from_set()
    expect_identical(class(es1), "data.frame")
    expect_identical(dim(es1), c(2L, 1L))
    expect_false(.is_tbl_elementset(es1))
})
