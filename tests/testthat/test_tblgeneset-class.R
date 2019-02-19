context("tblgeneset-class")

test_that("'tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, B = LETTERS)
    expect_s3_class(gs, "tbl_geneset")
    expect_identical(dim(gs), c(52L,2L))
    expect_true(is_tbl_geneset(gs))

    gs <- tbl_geneset()
    expect_s3_class(gs, "tbl_geneset")
    expect_identical(dim(gs), c(0L,2L))
    expect_true(is_tbl_geneset(gs))

    gs <- tbl_geneset(a = character(), b = LETTERS)
    expect_s3_class(gs, "tbl_geneset")
    expect_identical(dim(gs), c(26L,2L))
    expect_true(is_tbl_geneset(gs))
    expect_identical(levels(gs$set), c("a", "b"))

    expect_error(tbl_geneset(a = 1:5, B = LETTERS))
    expect_error(tbl_geneset(a = 1:5))
    expect_error(tbl_geneset(letters))
    expect_error(tbl_geneset(a = letters, LETTERS))
})

test_that("'is_tbl_geneset()' works", {
    expect_true(is_tbl_geneset(tibble(gene = character(), set = factor())))
    expect_true(is_tbl_geneset(tibble(gene = character(), set = factor(), x = integer())))
    expect_false(is_tbl_geneset(tibble(gene = integer(), set = factor())))
    expect_false(is_tbl_geneset(tibble(gene = character(), set = integer())))
    expect_false(is_tbl_geneset(tibble()))
    expect_false(is_tbl_geneset(tibble(gene = character())))
})

test_that("'select.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)

    gs1 <- select(gs, c(gene, set))
    expect_true(is_tbl_geneset(gs1))
    expect_identical(dim(select(gs, c(gene, set))), c(52L, 2L))
    expect_identical(select(gs, c(gene, set)), gs)

    gs2 <- select(gs, gene)
    expect_false(is_tbl_geneset(gs2))
    expect_identical(dim(select(gs, gene)), c(52L, 1L))
    expect_identical(select(gs, gene), gs["gene"])

    gs3 <- select(gs, set)
    expect_false(is_tbl_geneset(gs3))
    expect_identical(dim(select(gs, set)), c(52L, 1L))
    expect_identical(select(gs, set), gs["set"])
})

test_that("'mutate.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)

    gs1 <- mutate(gs, z = 1:52)
    expect_true(is_tbl_geneset(gs1))
    expect_identical(dim(gs1), c(52L, 3L))

    gs2 <- mutate(gs, gene = 1:52)
    expect_false(is_tbl_geneset(gs2))
    expect_identical(dim(gs2), c(52L, 2L))

    gs3 <- mutate(gs, set = 1:52)
    expect_false(is_tbl_geneset(gs3))
    expect_identical(dim(gs3), c(52L, 2L))
    
    expect_error(
        mutate(gs, z = 1:2),
        "Column `z` must be length 52 \\(the number of rows\\) or one, not 2"
    )
})

test_that("'group_by.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)

    gs1 <- group_by(gs, gene, set)
    expect_true(is_tbl_geneset(gs1))
    expect_identical(dim(gs1), c(52L, 2L))
    
    gs2 <- group_by(gs, gene)
    expect_true(is_tbl_geneset(gs2))
    expect_identical(dim(gs2), c(52L, 2L))

    gs3 <- group_by(gs, set)
    expect_true(is_tbl_geneset(gs3))
    expect_identical(dim(gs3), c(52L, 2L))
})

test_that("'ungroup.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    
    gs1 <- gs %>% group_by(gene) %>% ungroup
    expect_true(is_tbl_geneset(gs1))
    expect_identical(gs1, gs)
    expect_identical(dim(gs1), c(52L, 2L))   
})

test_that("'summarise.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)

    gs1 <- summarise(gs, n = n())
    expect_false(is_tbl_geneset(gs1))
    expect_identical(dim(gs1), c(1L, 1L))
    expect_equal(gs1$n, 52L)

    gs2 <- gs %>% group_by(gene) %>% summarise(n = n())
    expect_false(is_tbl_geneset(gs2))
    expect_identical(dim(gs2), c(52L, 2L))
    expect_equal(gs2$n, c(rep(1L, 52)))

    gs3 <- gs %>% group_by(set) %>% summarise(n = n())
    expect_false(is_tbl_geneset(gs3))
    expect_identical(dim(gs3), c(2L, 2L))
    expect_equal(gs3$n, c(26L, 26L))
})

test_that("'arrange.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)

    gs1 <- arrange(gs)
    expect_true(is_tbl_geneset(gs1))
    expect_equal(gs1, gs)

    gs2 <- arrange(gs, gene)
    expect_true(is_tbl_geneset(gs2))
    expect_equal(gs2, gs)
    expect_equal(gs2$gene, sort(gs$gene))

    gs3 <- arrange(gs, set)
    expect_true(is_tbl_geneset(gs3))
    expect_equal(gs3, gs)
    expect_equal(gs3$set, sort(gs$set))  

    gs4 <- arrange(gs, gene == "k")
    expect_true(is_tbl_geneset(gs4))
    expect_identical(dim(gs4), c(52L, 2L))    

    gs5 <- arrange(gs, set == "a")
    expect_true(is_tbl_geneset(gs5))
    expect_identical(dim(gs5), c(52L, 2L))
})
