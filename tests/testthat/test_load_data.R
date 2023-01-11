#### compile projections function ####
test_that("Test compile_projections: simple",{
  l2r <- c("1", "3")
  pp <- data.table(round = c(1,3),
                   target_end_date = c("2022-01-01", "2022-01-02"))
  sr <- data.table(round = c(1,3),
                   scenario_id = c("A", "A"))
  d <- list()
  d[[1]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-01",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  d[[2]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-02",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  expected <- rbindlist(list(d[[1]] %>%
                               .[, ":=" (round = 1,
                                         target_end_date= as.IDate(target_end_date),
                                         target = "inc case")],
                             d[[2]] %>%
                               .[, ":=" (round = 3,
                                         target_end_date= as.IDate(target_end_date),
                                         target = "inc case")]))
  expect_equal(compile_projections(proj = d, list2round = l2r,
                                   proj_period_key = pp,
                                   scenario_round_key = sr),
               expected)
})

test_that("Test compile_projections: only keep 1 round",{
  l2r <- c("1", "3")
  pp <- data.table(round = c(1,3),
                   target_end_date = c("2022-01-01", "2022-01-02"))
  sr <- data.table(round = c(1,3),
                   scenario_id = c("A", "A"))
  d <- list()
  d[[1]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-01",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  d[[2]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-02",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  expected <- d[[1]] %>%
    .[, ":=" (round = 1,
              target_end_date= as.IDate(target_end_date),
              target = "inc case")]
  expect_equal(compile_projections(proj = d, list2round = l2r,
                                   proj_period_key = pp,
                                   scenario_round_key = sr,
                                   rounds_to_include = 1),
               expected)
})

test_that("Test compile_projections: exclude R1",{
  l2r <- c("1", "3", "2")
  pp <- data.table(round = c(1,3,2),
                   target_end_date = c("2022-01-01", "2022-01-02", "2022-01-03"))
  sr <- data.table(round = c(1,3,2),
                   scenario_id = c("A", "A", "B"))
  d <- list()
  d[[1]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-01",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  d[[2]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-02",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  d[[3]] <- data.table(model_name = "Ensemble",
                       scenario_id = "B",
                       target_end_date = "2022-01-03",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  expected <- rbindlist(list(d[[1]] %>%
                               .[, ":=" (round = 1,
                                         target_end_date= as.IDate(target_end_date),
                                         target = "inc case")],
                             d[[2]] %>%
                               .[, ":=" (round = 3,
                                         target_end_date= as.IDate(target_end_date),
                                         target = "inc case")]))
  expect_equal(compile_projections(proj = d, list2round = l2r,
                                   proj_period_key = pp,
                                   scenario_round_key = sr,
                                   rounds_to_include = c(1,3)),
               expected)
})

test_that("Test compile_projections: exclude R10 (non-public)",{
  l2r <- sort(as.character(1:16))
  pp <- data.table(round = c(1,2,3),
                   target_end_date = c("2022-01-01", "2022-01-02", "2022-01-03"))
  sr <- data.table(round = c(1,2,3),
                   scenario_id = c("A", "B", "C"))
  d <- list()
  d[[1]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-01",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  d[[2]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-01",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  expected <- data.table(model_name = "Ensemble",
                         scenario_id = "A",
                         target_end_date = as.IDate("2022-01-01"),
                         location = "US",
                         target = "inc case",
                         quantile = c(0.01, 0.1, 0.2, 0.5),
                         value = c(0,1,2, 4),
                         round = 1)
  expect_equal(compile_projections(proj = d, list2round = l2r,
                                   proj_period_key = pp,
                                   scenario_round_key = sr),
               expected)
})



#### helpers ####
test_that("Test process_single_proj_element: 1 element in list",{
  l2r <- sort(as.character(1:16))
  d <- list()
  d[[1]] <- data.table(model_name = "Ensemble",
                  scenario_id = "A",
                  target_end_date = "2022-01-01",
                  location = "US",
                  target = "1 wk ahead inc case",
                  quantile = c(0.01, 0.1, 0.2, 0.5),
                  value = c(0,1,2, 4))
  i = 1
  expected <- data.table(model_name = "Ensemble",
                         scenario_id = "A",
                         target_end_date = as.IDate("2022-01-01"),
                         location = "US",
                         target = "inc case",
                         quantile = c(0.01, 0.1, 0.2, 0.5),
                         value = c(0,1,2, 4),
                         round = 1)
  expect_equal(process_single_proj_element(i = i, proj = d, list2round = l2r),
               expected)
})

test_that("Test process_single_proj_element: 2 elements in list",{
  l2r <- sort(as.character(1:16))
  d <- list()
  d[[1]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-01",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  d[[2]] <- data.table(model_name = "Ensemble",
                       scenario_id = "A",
                       target_end_date = "2022-01-01",
                       location = "US",
                       target = "1 wk ahead inc case",
                       quantile = c(0.01, 0.1, 0.2, 0.5),
                       value = c(0,1,2, 4))
  i = 2
  expected <- data.table(model_name = "Ensemble",
                         scenario_id = "A",
                         target_end_date = as.IDate("2022-01-01"),
                         location = "US",
                         target = "inc case",
                         quantile = c(0.01, 0.1, 0.2, 0.5),
                         value = c(0,1,2, 4),
                         round = 10)
  expect_equal(process_single_proj_element(i = i, proj = d, list2round = l2r),
               expected)
})



