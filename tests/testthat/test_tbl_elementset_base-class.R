context("tbl_elementset_base-class")

test_that("'subclass_tbl_elementset_base()' works", {
    tbl <- c("tbl_elementset_base", "tbl_df", "tbl", "data.frame")

    es1 <- .tbl_elementset(set1 = letters, set2 = LETTERS)
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", tbl))
    expect_length(class(es1), 5)

    es2 <- .tbl_element(es1)
    expect_s3_class(es2, "tbl_element")
    expect_identical(class(es2), c("tbl_element", tbl))
    expect_length(class(es2), 5)

    es3 <- .tbl_set(es1)
    expect_s3_class(es3, "tbl_set")
    expect_identical(class(es3), c("tbl_set", tbl))
    expect_length(class(es3), 5)
})

test_that("'filter.tbl_elementset_base()' works", {
    es <- .tbl_elementset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_elementset_base", "tbl_df", "tbl", "data.frame")

    es1 <- es %>% filter(element == "a")
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", tbl))
    expect_length(class(es1), 5)


    es2 <- .tbl_element(es) %>% filter(element == "a" | element == "A")
    expect_s3_class(es2, "tbl_element")
    expect_identical(class(es2), c("tbl_element", tbl))
    expect_length(class(es2), 5)

    es3 <- .tbl_set(es) %>% filter(set == "set1")
    expect_s3_class(es3, "tbl_set")
    expect_identical(class(es3), c("tbl_set", tbl))
    expect_length(class(es3), 5)

    es_not <- es %>% filter(element == "1")
    expect_identical(dim(es_not), c(0L, 2L))
})

test_that("'select.tbl_elementset_base()' works", {
    es <- .tbl_elementset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_elementset_base", "tbl_df", "tbl", "data.frame")

    es1 <- es %>% select(element)
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", tbl))
    expect_length(class(es1), 5)
    expect_error(es %>% select(Element))

    es2 <- .tbl_element(es) %>% select(element)
    expect_s3_class(es2, "tbl_element")
    expect_identical(class(es2), c("tbl_element", tbl))
    expect_length(class(es2), 5)
    expect_error(.tbl_element(es) %>% select(set))

    es3 <- .tbl_set(es) %>% select(set)
    expect_s3_class(es3, "tbl_set")
    expect_identical(class(es3), c("tbl_set", tbl))
    expect_length(class(es3), 5)
    expect_error(.tbl_set(es) %>% select(element))
})

test_that("'mutate.tbl_elementset_base()' works", {
    es <- .tbl_elementset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_elementset_base", "tbl_df", "tbl", "data.frame")

    es1 <- es %>% mutate(pval = rnorm(1:52))
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", tbl))
    expect_length(class(es1), 5)
    expect_error(es %>% mutate(pval = rnorm(1:10)))

    es2 <- .tbl_element(es) %>% mutate(pval = rnorm(1:52))
    expect_s3_class(es2, "tbl_element")
    expect_identical(class(es2), c("tbl_element", tbl))
    expect_length(class(es2), 5)
    expect_error(.tbl_element(es) %>% mutate(pval = rnorm(1:2)))

    es3 <- .tbl_set(es) %>% mutate(pval = rnorm(1:2))
    expect_s3_class(es3, "tbl_set")
    expect_identical(class(es3), c("tbl_set", tbl))
    expect_length(class(es3), 5)
    expect_error(.tbl_set(es) %>% mutate(pval = rnorm(1:23)))
})

test_that("'.tbl_nongroup_vars.tbl_elementset_base()' works", {
    es <- .tbl_elementset(set1 = letters, set2 = LETTERS)

    es1 <- es %>% .tbl_nongroup_vars()
    expect_is(es1, "character")
    expect_length(es1, 2)
    expect_error(es %>% .tbl_nongroup_vars(set))

    es2 <- .tbl_element(es) %>% .tbl_nongroup_vars()
    expect_is(es2, "character")
    expect_length(es2, 1)
    expect_error(es %>% .tbl_nongroup_vars(element))

    es3 <- .tbl_set(es) %>% .tbl_nongroup_vars()
    expect_is(es3, "character")
    expect_length(es3, 1)
    expect_error(es %>% .tbl_nongroup_vars(element))
})

test_that("'summarise.tbl_elementset_base()' works", {
    es <- .tbl_elementset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_elementset_base", "tbl_df", "tbl", "data.frame")

    es1 <- es %>% select(set) %>% summarise(n = n())
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", tbl))
    expect_length(class(es1), 5)
    expect_error(es %>% select(set) %>% summarise(set))

    es2 <- .tbl_element(es) %>% select(element) %>% summarise(n = n())
    expect_s3_class(es2,"tbl_element")
    expect_identical(class(es2), c("tbl_element", tbl))
    expect_length(class(es2), 5)
    expect_error(es %>% select(element) %>% summarise(element))

    es3 <- .tbl_set(es) %>% select(set) %>% summarise(n = n())
    expect_s3_class(es3, "tbl_set")
    expect_identical(class(es3), c("tbl_set", tbl))
    expect_length(class(es3), 5)
    expect_error(es %>% select(set) %>% summarise(set))
})

test_that("'arrange.tbl_elementset_base()' works", {
    es <- .tbl_elementset(set1 = letters, set2 = LETTERS)
    tbl <- c("tbl_elementset_base", "tbl_df", "tbl", "data.frame")

    es1 <- es %>% arrange(desc(element))
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", tbl))
    expect_length(class(es1), 5)
    expect_error(es %>% arrange(Element))

    es2 <- .tbl_element(es) %>% arrange(desc(element))
    expect_s3_class(es2, "tbl_element")
    expect_identical(class(es2), c("tbl_element", tbl))
    expect_length(class(es2), 5)
    expect_error(.tbl_element(es) %>% arrange(set))

    es3 <- .tbl_set(es) %>% arrange(desc(set))
    expect_s3_class(es3, "tbl_set")
    expect_identical(class(es3), c("tbl_set", tbl))
    expect_length(class(es3), 5)
    expect_error(.tbl_set(es) %>% arrange(element))
})
