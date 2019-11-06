context("BiocSet-methods")

test_that("'es_activate.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    expect_identical(.active(es), "elementset")

    es1 <- es %>% es_activate(element)
    expect_identical(.active(es1), "element")

    es2 <- es %>% es_activate(set)
    expect_identical(.active(es2), "set")

})

test_that("'filter.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)
    expect_equivalent(filter(es), es)

    es1 <- es %>% filter(set == "set1")
    expect_true(.is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_set(es1)), c(1L,1L))
    expect_identical(dim(es_elementset(es1)), c(26L,2L))
    expect_identical(es_elementset(es1)$element, letters)

    foo <- "k"
    es2 <- es %>% filter(element == foo)
    expect_true(.is_tbl_elementset(es_elementset(es2)))
    expect_identical(dim(es_element(es2)), c(1L,1L))
    expect_identical(es_elementset(es2)$element, foo)

    expect_error(es %>% filter(a))
})

test_that("'select.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% select(c(element, set))
    expect_true(.is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L,2L))
    expect_identical(es1, es)

    es2 <- es %>% select(element)
    expect_true(.is_tbl_elementset(es_elementset(es2)))
    expect_identical(dim(es_elementset(es2)), c(52L,2L))
    expect_identical(es_element(es2), es_element(es))

    es3 <- es %>% select(set)
    expect_true(.is_tbl_elementset(es_elementset(es3)))
    expect_identical(dim(es_elementset(es3)), c(52L, 2L))
    expect_identical(es_set(es3), es_set(es))

    expect_error(es %>% select(a))
})

test_that("'mutate.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% mutate(pval = rnorm(1:52))
    expect_true(.is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L,3L))

    expect_error(es %>% mutate(z = 1:2))
    expect_error(es %>% mutate(1:2))
    expect_error(es %>% mutate(a))
})

test_that("'summarise.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% summarise(n = n())
    expect_false(.is_tbl_elementset(es1))
    expect_identical(dim(es1), c(1L,1L))
    expect_equal(es1$n, 52L)

    es2 <- es %>% group_by(element) %>% summarise(n = n())
    expect_false(.is_tbl_elementset(es2))
    expect_identical(dim(es2), c(52L,2L))
    expect_equal(es2$n, c(rep(1L, 52)))

    es3 <- es %>% group_by(set) %>% summarise(n = n())
    expect_false(.is_tbl_elementset(es3))
    expect_identical(dim(es3), c(2L,2L))
    expect_equal(es3$n, c(26L, 26L))

    es4 <- es %>% group_by(set) %>% summarize(n = n())
    expect_false(.is_tbl_elementset(es4))
    expect_identical(dim(es4), c(2L, 2L))
    expect_equal(es4$n, c(26L, 26L))

    expect_error(es %>% summarise(a))
})

test_that("'arrange.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% arrange()
    expect_true(.is_tbl_elementset(es_elementset(es1)))
    expect_equal(es1, es)

    es2 <- es %>% arrange(element)
    expect_true(.is_tbl_elementset(es_elementset(es2)))
    expect_equal(es2, es)
    expect_equal(es_elementset(es2)$element, sort(es_elementset(es)$element))

    es3 <- es %>% arrange(set)
    expect_true(.is_tbl_elementset(es_elementset(es3)))
    expect_equal(es3, es)
    expect_equal(es_elementset(es3)$set, sort(es_elementset(es)$set))

    es4 <- es %>% arrange(element == "k")
    expect_true(.is_tbl_elementset(es_elementset(es4)))
    expect_identical(dim(es_elementset(es4)), c(52L,2L))

    es5 <- es %>% arrange(set == "set1")
    expect_true(.is_tbl_elementset(es_elementset(es5)))
    expect_identical(dim(es_elementset(es5)), c(52L,2L))
})

test_that("'.tbl_nongroup_vars.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es_elementset(es) %>% .tbl_nongroup_vars()
    expect_identical(es1, c("element", "set"))
    expect_identical(length(es1), 2L)
    expect_identical(class(es1), "character")

    es2 <- es_element(es) %>% .tbl_nongroup_vars()
    expect_identical(es2, "element")
    expect_identical(length(es2), 1L)
    expect_identical(class(es2), "character")
})

test_that("'left_join.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)
    tbl <- tibble(x = rnorm(1:52), y = c(letters, LETTERS))

    es1 <- es %>% left_join(tbl, by = c(element = "y"))
    expect_true(.is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L, 3L))

    expect_error(es %>% left_join())
    expect_error(es %>% left_join(tbl))
    expect_error(es %>% left_join(tbl, by = 1:4))
    expect_error(es %>% left_join(tbl, by = "element"))
    expect_error(es %>% left_join(tbl, by = c("y" = element)))
})

test_that("'as.list.BiocSet()' works", {
    library(org.Hs.eg.db) 
    es <- go_sets(org.Hs.eg.db, "ENSEMBL")

    es1 <- as.list(es)
    expect_identical(class(es1), "list")
    expect_gte(length(es1), 18175L)
    expect_identical(names(es1), es_set(es)$set)
})

test_that("'union.BiocSet()' works",
{
    es1 <- BiocSet(set1 = letters[c(1:3)], set2 = LETTERS[c(1:3)])
    es2 <- BiocSet(set1 = letters[c(2:6)], set2 = LETTERS[c(2:6)])

    tbl <- union(es1, es2)
    expect_identical(dim(es_element(tbl)), c(12L, 1L))
    expect_identical(dim(es_set(tbl)), c(2L, 1L))
    expect_identical(dim(es_elementset(tbl)), c(12L, 2L))
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
