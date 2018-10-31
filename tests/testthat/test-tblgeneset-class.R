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

test_that("'filter.tbl_geneset' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    expect_equivalent(filter(gs), gs)
    
    gs1 <- filter(gs, set == "a")
    expect_true(is_tbl_geneset(gs1))
    expect_identical(dim(gs1), c(26L, 2L))
    expect_identical(gs1$gene, letters)

    exp <- "k"
    gs2 <- filter(gs, gene == exp)
    expect_true(is_tbl_geneset(gs2))
    expect_identical(dim(gs2), c(1L, 2L))
    expect_identical(gs2$gene, exp)
})

test_that("'select.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    expect_identical(dim(select(gs, gene)), c(52L, 1L))
    expect_identical(dim(select(gs, set)), c(52L, 1L))
    expect_identical(dim(select(gs, c(gene, set))), c(52L, 2L))
    expect_identical(select(gs, gene), gs["gene"])
    expect_identical(select(gs, set), gs["set"])
    expect_identical(select(gs, c(gene, set)), gs)
})

test_that("'mutate.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    expect_identical(dim(mutate(gs, z = 1:52)), c(52L, 3L))
    
    expect_error(
        mutate(gs, z = 1:2),
        "Column `z` must be length 52 \\(the number of rows\\) or one, not 2"
    )
})

test_that("'group_by.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    expect_identical(names(group_by(gs, gene)), c("gene", "set"))
    expect_identical(names(group_by(gs, set)), c("gene", "set"))
    expect_identical(dim(group_by(gs, gene)), c(52L, 2L))
    expect_identical(dim(group_by(gs, set)), c(52L, 2L))
})

test_that("'ungroup.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    grps <- gs %>% group_by(gene)
    expect_identical(ungroup(grps), gs)
    expect_identical(dim(ungroup(grps)), c(52L, 2L))
    expect_identical(names(ungroup(grps)), c("gene", "set"))    
})

test_that("'summarise.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    expect_identical(dim(summarise(gs, n = n())), c(1L, 1L))
    expect_identical(names(summarise(gs, n = n())), "n")
    expect_equal(summarise(gs, n = n())$n, 52L)

    grps <- gs %>% group_by(set)
    expect_identical(dim(summarise(grps, n = n())), c(2L, 2L))
    expect_identical(names(summarise(grps, n = n())), c("set", "n"))
    expect_equal(summarise(grps, n = n())$n, c(26L, 26L))

    grps2 <- gs %>% group_by(gene)
    expect_identical(dim(summarise(grps2, n = n())), c(52L, 2L))
    expect_identical(names(summarise(grps2, n = n())), c("gene", "n"))
    expect_equal(summarise(grps2, n = n())$n, c(rep(1L, 52)))    
})

test_that("'arrange.tbl_geneset()' works", {
    gs <- tbl_geneset(a = letters, b = LETTERS)
    expect_equal(arrange(gs), gs)
    expect_equal(arrange(gs, gene), gs)
    expect_equal(arrange(gs, set), gs)
    expect_equal(arrange(gs, gene)$gene, sort(gs$gene))
    expect_equal(arrange(gs, set)$set, sort(gs$set))    
    expect_identical(dim(arrange(gs, set == "a")), c(52L, 2L))
    expect_identical(dim(arrange(gs, gene == "k")), c(52L, 2L))
    expect_identical(names(arrange(gs, set == "a")), c("gene", "set"))
    expect_identical(names(arrange(gs, gene == "k")), c("gene", "set"))
})
