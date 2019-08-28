context("BiocSet")

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

test_that("'.element()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% es_element()
    expect_s3_class(es1, "tbl_element")
    expect_identical(class(es1), c("tbl_element", "tbl_elementset_base",
                                   "tbl_df", "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(dim(es1), c(52L,1L))
})

test_that("'.set()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% es_set()
    expect_s3_class(es1, "tbl_set")
    expect_identical(class(es1), c("tbl_set", "tbl_elementset_base",
                                  "tbl_df", "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(dim(es1), c(2L,1L))
})

test_that("'.elementset()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% es_elementset()
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(class(es1), c("tbl_elementset", "tbl_elementset_base",
                                   "tbl_df", "tbl", "data.frame"))
    expect_length(class(es1), 5)
    expect_identical(dim(es1), c(52L,2L))
})

test_that("'.active()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% .active()
    expect_is(es1, "character")
    expect_length(es1, 1)
})

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
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_set(es1)), c(1L,1L))
    expect_identical(dim(es_elementset(es1)), c(26L,2L))
    expect_identical(es_elementset(es1)$element, letters)

    foo <- "k"
    es2 <- es %>% filter(element == foo)
    expect_true(is_tbl_elementset(es_elementset(es2)))
    expect_identical(dim(es_element(es2)), c(1L,1L))
    expect_identical(es_elementset(es2)$element, foo)

    expect_error(es %>% filter(a))
})

test_that("'select.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% select(c(element, set))
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L,2L))
    expect_identical(es1, es)

    es2 <- es %>% select(element)
    expect_true(is_tbl_elementset(es_elementset(es2)))
    expect_identical(dim(es_elementset(es2)), c(52L,2L))
    expect_identical(es_element(es2), es_element(es))

    es3 <- es %>% select(set)
    expect_true(is_tbl_elementset(es_elementset(es3)))
    expect_identical(dim(es_elementset(es3)), c(52L, 2L))
    expect_identical(es_set(es3), es_set(es))

    expect_error(es %>% select(a))
})

test_that("'mutate.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% mutate(pval = rnorm(1:52))
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L,3L))

    expect_error(es %>% mutate(z = 1:2))
    expect_error(es %>% mutate(1:2))
    expect_error(es %>% mutate(a))
})

test_that("'.normalize_mapping()' works", {
    es <- .normalize_mapping(letters, LETTERS)
    expect_s3_class(es, "tbl_df")
    expect_false(is_tbl_elementset(es))
    expect_identical(dim(es), c(26L, 2L))
    expect_true(is.character(es$element))
    expect_true(is.character(es$to))

    es1 <- .normalize_mapping(
        list(letters[1], letters[2:3], character()),
        LETTERS[1:3]
    )
    expect_s3_class(es1, "tbl_df")
    expect_false(is_tbl_elementset(es1))
    expect_identical(dim(es1), c(3L, 2L))
    expect_true(is.character(es$element))
    expect_true(is.character(es$to))

    es2 <- .normalize_mapping(
        letters[1:3],
        list(LETTERS[1], LETTERS[2:3], character())
    )
    expect_s3_class(es2, "tbl_df")
    expect_false(is_tbl_elementset(es2))
    expect_identical(dim(es2), c(3L, 2L))
    expect_true(is.character(es$element))
    expect_true(is.character(es$to))

    es3 <- .normalize_mapping(
        list(letters[1:2], letters[3], character()),
        list(LETTERS[1], LETTERS[2:3], character())
    )
    expect_s3_class(es3, "tbl_df")
    expect_false(is_tbl_elementset(es3))
    expect_identical(dim(es3), c(4L, 2L))
    expect_true(is.character(es$element))
    expect_true(is.character(es$to))
})

test_that("'map_element.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    # 1:1 mapping
    es1 <- es %>% map_element(letters, LETTERS)
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_element(es1)), c(26L, 1L))
    expect_identical(dim(es_set(es1)), c(2L, 1L))
    expect_identical(dim(es_elementset(es1)), c(52L,2L))

    # A single set example with metadata in each tibble
    element <- tibble(element = letters[1:5], foo = 1:5)
    set <- tibble(set = "set1", bar = 1)
    elementset <- tibble(element = letters[1:5], set = rep("set1", 5), baz = 1:5)
    .data = BiocSet_from_elementset(elementset, element, set)

    from = c("a", "b", "b", "c", "d")
    to = c("A", "B", "C", "D", "D")
    # 1:1 mapping, 1:many mapping, many:1 mapping
    es2 <- map_element(.data, from, to)
    expect_s4_class(es2, "BiocSet")
    expect_identical(dim(es_element(es2)), c(5L, 2L))
    expect_identical(dim(es_set(es2)), c(1L, 2L))
    expect_identical(dim(es_elementset(es2)), c(5L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es2)))
    expect_true(is.list(es_element(es2)$foo))
    expect_true(is.double(es_set(es2)$bar))
    expect_true(is.list(es_elementset(es2)$baz))
 
    # 1:1 mapping, 1:many mapping, many:1 mapping, dropping unmapped element(s)
    es3 <- map_element(.data, from, to, keep_unmapped = FALSE)
    expect_s4_class(es3, "BiocSet")
    expect_identical(dim(es_element(es3)), c(4L, 2L))
    expect_identical(dim(es_set(es3)), c(1L, 2L))
    expect_identical(dim(es_elementset(es3)), c(4L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es3)))
    expect_true(is.list(es_element(es3)$foo))
    expect_true(is.double(es_set(es3)$bar))
    expect_true(is.list(es_elementset(es3)$baz))

    # multiple sets with metadata in element and elementset tibble
    elementset <- tibble(
        element = c("a", "b", "c"),
        set = c("set1", "set1", "set2")
    )
    .data = BiocSet_from_elementset(elementset) %>%
        mutate_element(foo = 1:3) %>%
        mutate_elementset(bar = 1:3)

    # 1:1 mapping, but in different sets
    from <- c("b", "c")
    to <- c("D", "E")
    es4 <- map_element(.data, from, to)
    expect_s4_class(es4, "BiocSet")
    expect_identical(dim(es_element(es4)), c(3L, 2L))
    expect_identical(dim(es_set(es4)), c(2L, 1L))
    expect_identical(dim(es_elementset(es4)), c(3L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es4)))
    expect_true(is.integer(es_element(es4)$foo))
    expect_true(is.integer(es_elementset(es4)$bar))

    # 1:1 mappping, in different sets, dropping unmapped element(s)
    es5 <- map_element(.data, from, to, keep_unmapped = FALSE)
    expect_s4_class(es5, "BiocSet")
    expect_identical(dim(es_element(es5)), c(2L, 2L))
    expect_identical(dim(es_set(es5)), c(2L, 1L))
    expect_identical(dim(es_elementset(es5)), c(2L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es5)))
    expect_true(is.integer(es_element(es5)$foo))
    expect_true(is.integer(es_elementset(es5)$bar))
    expect_identical(to, es_element(es5)$element)
    expect_identical(to, es_elementset(es5)$element)

    # many:1 mapping, in different sets
    from <- c("b", "c")
    to <- c("D", "D")
    es6 <- map_element(.data, from, to)
    expect_s4_class(es6, "BiocSet")
    expect_identical(dim(es_element(es6)), c(2L, 2L))
    expect_identical(dim(es_set(es6)), c(2L, 1L))
    expect_identical(dim(es_elementset(es6)), c(3L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es6)))
    expect_true(is.list(es_element(es6)$foo))
    expect_true(is.integer(es_elementset(es6)$bar))

    # many:1 mapping, in different sets, dropping unmapped element(s)
    es7 <- map_element(.data, from, to, keep_unmapped = FALSE)
    expect_s4_class(es7, "BiocSet")
    expect_identical(dim(es_element(es7)), c(1L, 2L))
    expect_identical(dim(es_set(es7)), c(2L, 1L))
    expect_identical(dim(es_elementset(es7)), c(2L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es7)))
    expect_true(is.list(es_element(es7)$foo))
    expect_true(is.integer(es_elementset(es7)$bar))
    expect_identical(to, es_elementset(es7)$element)

    # 1:many mapping, in different sets
    from <- c("b", "b", "c", "c")
    to <- c("D", "E", "F", "G")
    es8 <- map_element(.data, from, to)
    expect_s4_class(es8, "BiocSet")
    expect_identical(dim(es_element(es8)), c(5L, 2L))
    expect_identical(dim(es_set(es8)), c(2L, 1L))
    expect_identical(dim(es_elementset(es8)), c(5L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es8)))
    expect_true(is.integer(es_element(es8)$foo))
    expect_true(is.integer(es_elementset(es8)$bar))

    # 1:many mapping, in different sets, dropping unmapped element(s)
    es9 <- map_element(.data, from, to, keep_unmapped = FALSE)
    expect_s4_class(es9, "BiocSet")
    expect_identical(dim(es_element(es9)), c(4L, 2L))
    expect_identical(dim(es_set(es9)), c(2L, 1L))
    expect_identical(dim(es_elementset(es9)), c(4L, 3L))
    expect_true(is_tbl_elementset(es_elementset(es9)))
    expect_true(is.integer(es_element(es9)$foo))
    expect_true(is.integer(es_elementset(es9)$bar))
    expect_identical(to, es_element(es9)$element)
    expect_identical(to, es_elementset(es9)$element)

    expect_error(es %>% map_element())
})

test_that("'map_set.BiocSet()' works", {
    es <- BiocSet(a = letters, B = LETTERS)

    es1 <- es %>% map_set("a", "A")
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L,2L))
    expect_identical(levels(es_elementset(es1)$set), levels(es_set(es1)$set))

    expect_error(es %>% map_set())
})

test_that("'summarise.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% summarise(n = n())
    expect_false(is_tbl_elementset(es1))
    expect_identical(dim(es1), c(1L,1L))
    expect_equal(es1$n, 52L)

    es2 <- es %>% group_by(element) %>% summarise(n = n())
    expect_false(is_tbl_elementset(es2))
    expect_identical(dim(es2), c(52L,2L))
    expect_equal(es2$n, c(rep(1L, 52)))

    es3 <- es %>% group_by(set) %>% summarise(n = n())
    expect_false(is_tbl_elementset(es3))
    expect_identical(dim(es3), c(2L,2L))
    expect_equal(es3$n, c(26L, 26L))

    expect_error(es %>% summarise(a))
})

test_that("'arrange.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% arrange()
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_equal(es1, es)

    es2 <- es %>% arrange(element)
    expect_true(is_tbl_elementset(es_elementset(es2)))
    expect_equal(es2, es)
    expect_equal(es_elementset(es2)$element, sort(es_elementset(es)$element))

    es3 <- es %>% arrange(set)
    expect_true(is_tbl_elementset(es_elementset(es3)))
    expect_equal(es3, es)
    expect_equal(es_elementset(es3)$set, sort(es_elementset(es)$set))

    es4 <- es %>% arrange(element == "k")
    expect_true(is_tbl_elementset(es_elementset(es4)))
    expect_identical(dim(es_elementset(es4)), c(52L,2L))

    es5 <- es %>% arrange(set == "set1")
    expect_true(is_tbl_elementset(es_elementset(es5)))
    expect_identical(dim(es_elementset(es5)), c(52L,2L))
})

test_that("'tbl_nongroup_vars.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es_elementset(es) %>% tbl_nongroup_vars()
    expect_identical(es1, c("element", "set"))
    expect_identical(length(es1), 2L)
    expect_identical(class(es1), "character")

    es2 <- es_element(es) %>% tbl_nongroup_vars()
    expect_identical(es2, "element")
    expect_identical(length(es2), 1L)
    expect_identical(class(es2), "character")
})

test_that("'left_join.BiocSet()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)
    tbl <- tibble(x = rnorm(1:52), y = c(letters, LETTERS))

    es1 <- es %>% left_join(tbl, by = c(element = "y"))
    expect_true(is_tbl_elementset(es_elementset(es1)))
    expect_identical(dim(es_elementset(es1)), c(52L, 3L))

    expect_error(es %>% left_join())
    expect_error(es %>% left_join(tbl))
    expect_error(es %>% left_join(tbl, by = 1:4))
    expect_error(es %>% left_join(tbl, by = "element"))
    expect_error(es %>% left_join(tbl, by = c("y" = element)))
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

test_that("'as.list.BiocSet()' works", {
    library(org.Hs.eg.db) 
    es <- go_sets(org.Hs.eg.db, "ENSEMBL")

    es1 <- as.list(es)
    expect_identical(class(es1), "list")
    expect_identical(length(es1), 17495L)
    expect_identical(names(es1), es_set(es)$set)
})

test_that("'tibble_by_elementset()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% tibble_by_elementset()
    expect_s3_class(es1, "tbl_elementset")
    expect_identical(dim(es1), c(52L, 2L))
    expect_true(is_tbl_elementset(es1))
})

test_that("'tibble_by_element()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% tibble_by_element()
    expect_s3_class(es1, "tbl_df")
    expect_identical(dim(es1), c(52L, 2L))
    expect_true(is_tbl_elementset(es1))
})

test_that("'tibble_by_set()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% tibble_by_set()
    expect_s3_class(es1, "tbl_df")
    expect_identical(dim(es1), c(2L, 2L))
    expect_false(is_tbl_elementset(es1))
})

test_that("'data.frame_by_elementset()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% data.frame_by_elementset()
    expect_identical(class(es1), "data.frame")
    expect_identical(dim(es1), c(52L, 2L))
    expect_true(is_tbl_elementset(es1))
})

test_that("'data.frame_by_element()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)

    es1 <- es %>% data.frame_by_element()
    expect_identical(class(es1), "data.frame")
    expect_identical(dim(es1), c(52L, 1L))
    expect_false(is_tbl_elementset(es1))
})

test_that("'data.frame_by_set()' works", {
    es <- BiocSet(set1 = letters, set2 = LETTERS)
    
    es1 <- es %>% data.frame_by_set()
    expect_identical(class(es1), "data.frame")
    expect_identical(dim(es1), c(2L, 1L))
    expect_false(is_tbl_elementset(es1))
})
