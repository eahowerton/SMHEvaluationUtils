suppressMessages({
  library(testthat)
  library(dplyr)
  library(tibble)
  library(data.table)
})


#### exclusion function ####
# TO ADD


#### exclusion helpers ####
test_that("Test exclude_NA_vals: NA quantiles",{
  d <- setDT(expand.grid(quantile = c(NA, 0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[!is.na(quantile) & !is.na(value)]
  expect_equal(exclude_NA_vals(d), expected)
})

test_that("Test exclude_NA_vals: NA values",{
  d <- setDT(expand.grid(quantile = c(0.1, 0.2, 0.5),
                         value = c(NA, 0,1,2)))
  expected <- d[!is.na(quantile) & !is.na(value)]
  expect_equal(exclude_NA_vals(d), expected)
})

test_that("Test exclude_NA_vals: NA quantiles and values",{
  d <- setDT(expand.grid(quantile = c(NA, 0.1, 0.2, 0.5),
                         value = c(NA, 0,1,2)))
  expected <- d[!is.na(quantile) & !is.na(value)]
  expect_equal(exclude_NA_vals(d), expected)
})

test_that("Test exclude_quantile01: 0 quantile",{
  d <- setDT(expand.grid(quantile = c(0, 0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[quantile != 0]
  expect_equal(exclude_quantile01(d), expected)
})

test_that("Test exclude_quantile01: 1 quantile",{
  d <- setDT(expand.grid(quantile = c(0.1, 0.2, 0.5, 1),
                         value = c(0,1,2)))
  expected <- d[quantile != 1]
  expect_equal(exclude_quantile01(d), expected)
})

test_that("Test exclude_quantile01: 0 and 1 quantile",{
  d <- setDT(expand.grid(quantile = c(0, 0.1, 0.2, 0.5, 1),
                         value = c(0,1,2)))
  expected <- d[!(quantile %in% c(0,1))]
  expect_equal(exclude_quantile01(d), expected)
})

test_that("Test exclude_non_public: round 8, 10",{
  d <- setDT(expand.grid(round = c(2,8,10),
                         quantile = c(0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[, e_non_public := ifelse(round %in% c(8,10), 1, 0)]
  expect_equal(exclude_non_public(d), expected)
})

test_that("Test exclude_territories: all territories",{
  d <- setDT(expand.grid(location = c("US", "60", "66", "69", "72", "74", "78"),
                         quantile = c(0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[, e_territories := ifelse(location %in% c("60", "66", "69", "72", "74", "78"), 1, 0)]
  expect_equal(exclude_territories(d), expected)
})

test_that("Test exclude_model_names: all territories",{
  d <- setDT(expand.grid(model_name = c("OliverWyman-Navigator",
                                        "IHME-IHME_COVID_model_deaths_unscaled",
                                        "Ensemble"),
                         quantile = c(0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[, e_model := ifelse(model_name %in% c("OliverWyman-Navigator",
                                                    "IHME-IHME_COVID_model_deaths_unscaled"), 1, 0)]
  expect_equal(exclude_model_names(d), expected)
})

test_that("Test exclude_non_monotonic: non-increasing CDF",{
  d <- data.table(model_name = "Ensemble",
                         round = 1,
                         scenario_name = "A",
                         target_end_date = "2022-01-01",
                         location = "US",
                         target = "inc case",
                         quantile = c(0.1, 0.2, 0.5),
                         value = c(0,1,2))
  d2 <- data.table(model_name = rep("rm",4),
                   round = 1,
                   scenario_name = "A",
                   target_end_date = "2022-01-01",
                   location = "US",
                   target = "inc case",
                   quantile = c(0.1,0.2, 0.5, 0.9),
                   value = c(3,1,2,4))
  expected <- rbindlist(list(d[, e_non_monotonic := 0],
                             d2[, e_non_monotonic := 1])) %>%
    .[order(quantile)]
  expect_equal(exclude_non_monotonic(rbindlist(list(d,d2))), expected)
})

test_that("Test exclude_outside_projperiod: wrong dates with each round",{
  pp <- data.table(round = c(1,2,3),
                   target_end_date = c("2022-01-01", "2022-01-02", "2022-01-03"))
  d <- setDT(expand.grid(round = 1:3,
                         target_end_date = c("2022-01-01", "2022-01-02", "2022-01-03"),
                         quantile = c(0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[, e_wrongdate := ifelse(round == 1 & target_end_date == "2022-01-01", 0, 1)]
  expected <- expected[, e_wrongdate := ifelse(round == 2 & target_end_date == "2022-01-02", 0,
                                               ifelse(round == 3 & target_end_date == "2022-01-03", 0, e_wrongdate))]
  expect_equal(exclude_outside_projperiod(d, pp), expected)
})

test_that("Test exclude_outside_projperiod: round not included",{
  pp <- data.table(round = c(1,2,3),
                   target_end_date = c("2022-01-01", "2022-01-02", "2022-01-03"))
  d <- setDT(expand.grid(round = 1:4,
                         target_end_date = c("2022-01-01", "2022-01-02", "2022-01-03"),
                         quantile = c(0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[, e_wrongdate := ifelse(round == 1 & target_end_date == "2022-01-01", 0, 1)]
  expected <- expected[, e_wrongdate := ifelse(round == 2 & target_end_date == "2022-01-02", 0,
                                               ifelse(round == 3 & target_end_date == "2022-01-03", 0, e_wrongdate))]
  expect_equal(exclude_outside_projperiod(d, pp), expected)
})

test_that("Test exclude_wrong_round: scenario/round not included",{
  sr <- data.table(round = c(1,2,3),
                   scenario_id = c("A", "B", "C"))
  d <- setDT(expand.grid(round = 1:3,
                         scenario_id = c("A", "B", "C"),
                         quantile = c(0.1, 0.2, 0.5),
                         value = c(0,1,2)))
  expected <- d[, e_wrong_round := ifelse(round == 1 & scenario_id == "A", 0, 1)]
  expected <- expected[, e_wrong_round := ifelse(round == 2 & scenario_id == "B", 0,
                                               ifelse(round == 3 & scenario_id == "C", 0, e_wrong_round))]
  expect_equal(exclude_wrong_round(d, sr), expected)
})





