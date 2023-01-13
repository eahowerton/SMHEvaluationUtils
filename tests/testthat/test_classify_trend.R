suppressMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(data.table)
})

#### full classification function ####
test_that("Test full_classif: no group by",{
  l <- 1
  pct_f <- 0.2
  d <- setDT(expand.grid(target = c("A", "B", "C"),
                         obs = 1:5,
                         target_end_date = "2022-01-01"))
  lwr <- quantile(log(2:5)-log(1:4), 0.4)
  upr <- quantile(log(2:5)-log(1:4), 0.6)
  expected <- copy(d)
  expected <- expected %>%
    .[, ":=" (pct_change = c(NA, log(2:5)-log(1:4)),
              lwr_quantile = lwr,
              upr_quantile = upr), by = .(target)] %>%
    .[, change_bin := ifelse(pct_change > upr_quantile, "inc",
                             ifelse(pct_change < lwr_quantile, "dec", "flat"))] %>%
    .[, .(target, pct_change, obs, target_end_date, lwr_quantile, upr_quantile, change_bin)] %>%
    .[order(target)]
  expect_equal(full_classif(dat = d, lag = l, pct_flat = pct_f), expected)
})

#### pct change function ####
test_that("Test calculate_pct_change: no group by",{
  l <- 1
  d <- data.table(obs = 1:5,
                  target_end_date = "2022-01-01")
  expected <- d[, ":=" (pct_change = c(NA, log(2:5)-log(1:4)))] %>%
    .[, .(pct_change, obs, target_end_date)]
  expect_equal(calculate_pct_change(d, l), expected)
})


test_that("Test calculate_pct_change: 0 value",{
  l <- 1
  d <- data.table(obs = 0:5,
                  target_end_date = "2022-01-01")
  expected <- d[, ":=" (pct_change = c(NA, log(1:5)-log(c(1,1:4))))] %>%
    .[, .(pct_change, obs, target_end_date)]
  expect_equal(calculate_pct_change(d, l), expected)
})

test_that("Test calculate_pct_change: group by",{
  l <- 1
  d <- setDT(expand.grid(grp1 = c("A", "B"),
                         grp2 = c("1", "2"),
                         obs = 0:5,
                         target_end_date = "2022-01-01")) %>%
    .[, obs := ifelse(grp1 == "A", obs*5+1, obs)] %>%
    .[, obs := ifelse(grp2 == "2", obs + 3, obs)]
  expected <- d[, ":=" (pct_change = c(NA, diff(log(ifelse(obs == 0, 1, obs)), lag = l))), by = .(grp1, grp2)] %>%
    .[, .(grp1, grp2, pct_change, obs, target_end_date)] %>%
    .[order(grp1)] %>%
    .[order(grp2)]
  expect_equal(calculate_pct_change(d, l, byvars = c("grp1", "grp2")), expected)
})


test_that("Test calculate_pct_change: group by",{
  l <- 1
  d <- setDT(expand.grid(grp1 = c("A", "B"),
                         grp2 = c("1", "2"),
                         obs = 0:5,
                         target_end_date = "2022-01-01")) %>%
    .[, obs := ifelse(grp1 == "A", obs*5+1, obs)] %>%
    .[, obs := ifelse(grp2 == "2", obs + 3, obs)]
  expected <- d[, ":=" (pct_change = c(NA, diff(log(ifelse(obs == 0, 1, obs)), lag = l))), by = .(grp1, grp2)] %>%
    .[, .(grp1, grp2, pct_change, obs, target_end_date)] %>%
    .[order(grp1)] %>%
    .[order(grp2)]
  expect_equal(calculate_pct_change(d, l, byvars = c("grp1", "grp2")), expected)
})


#### threshold function ####
test_that("Test calc_thresh: simple",{
  pct_f = 0.3
  d <- setDT(expand.grid(target = c("A", "B", "C"),
                         pct_change = 0:100))
  expected <- data.table(target = as.factor(c("A", "B", "C")),
                         lwr_quantile = 35,
                         upr_quantile = 65)
  expect_equal(calc_thresh(d, pct_f), expected)
})


test_that("Test calc_thresh: NA included",{
  pct_f <- 0.3
  d <- setDT(expand.grid(target = c("A", "B", "C"),
                         pct_change = c(NA,0:100)))
  expected <- data.table(target = as.factor(c("A", "B", "C")),
                         lwr_quantile = 35,
                         upr_quantile = 65)
  expect_equal(calc_thresh(d, pct_f), expected)
})

#### classif function ####
test_that("Test classify_incdec: simple",{
  lwr = -0.25
  upr = 0.25
  thresh <- data.table(target = c("A", "B", "C"),
                     lwr_quantile = lwr,
                     upr_quantile = upr)
  d <- setDT(expand.grid(target = c("A", "B", "C"),
                         pct_change = c(-0.5,0,0.5)))
  expected <- copy(d)
  expected <- expected %>%
    .[, ":=" (target=as.character(target),
              lwr_quantile = lwr,
              upr_quantile = upr,
              change_bin = ifelse(pct_change == -0.5, "dec",
                                  ifelse(pct_change == 0, "flat", "inc"))
              )] %>%
    .[order(target)]
  attr(expected, 'out.attrs') <- NULL
  expect_equal(classify_incdec(dat_change = d, thresh = thresh), expected)
})

