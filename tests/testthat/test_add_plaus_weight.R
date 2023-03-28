
### weekly plaus function ####
test_that("Test plaus_weight_by_week: round 1 alpha exclusion",{
  p <- data.table(round = c(1,2,3),
                   target_end_date = c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01"))
  vtd <- c("2022-01-15", "2022-02-15", "2022-03-15")
  names(vtd) <- c("alpha", "delta", "omicron")
  expected <- p[, plaus_week := ifelse(round == 1 & target_end_date == "2022-01-01", 1, 0)]
  expect_equal(plaus_weight_by_week(variant_takeover = vtd, proj = p), expected)
})

test_that("Test plaus_weight_by_week: round 5 delta exclusion",{
  p <- data.table(round = c(1,2,5),
                  target_end_date = c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01"))
  vtd <- c("2022-01-15", "2022-02-15", "2022-03-15")
  names(vtd) <- c("alpha", "delta", "omicron")
  expected <- p[, plaus_week := ifelse(round %in% 1:5 & target_end_date %in% c("2022-01-01", "2022-02-01"), 1, 0)]
  expect_equal(plaus_weight_by_week(variant_takeover = vtd, proj = p), expected)
})


test_that("Test plaus_weight_by_week: round 9 omicron exclusion",{
  p <- data.table(round = c(1,2,9,11),
                  target_end_date = c("2022-01-01", "2022-02-01", "2022-03-01", "2022-04-01"))
  vtd <- c("2022-01-15", "2022-02-15", "2022-03-15")
  names(vtd) <- c("alpha", "delta", "omicron")
  expected <- p[, plaus_week := ifelse(target_end_date == "2022-04-01", 0, 1)]
  expect_equal(plaus_weight_by_week(variant_takeover = vtd, proj = p), expected)
})

### model name and round ###

test_that("Test plaus_weight_by_model_name: exclude model",{
  p <- data.table(round = c(1,1,1,1),
                  scenario_id = paste0(LETTERS[1:4], "-2020-02-01"),
                  model_name = rep("A", 4))
  exc <- data.frame(model_name = "A",
                    round = 1,
                    weight = 0)
  expected <- p[, plaus_mod := 0]
  expect_equal(plaus_weight_by_model_name(modelname_round_weight = exc, proj = p), expected)
})

test_that("Test plaus_weight_by_model_name: half weight",{
  p <- expand.grid(round = 1,
                  scenario_id = paste0(LETTERS[1:4], "-2020-02-01"),
                  model_name = c("A", "B")) %>%
    setDT()
  exc <- data.frame(model_name = c("A","B"),
                    round = 1,
                    weight = c(0.5,0.5))
  expected <- p[, plaus_mod := 0.5]
  expect_equal(plaus_weight_by_model_name(modelname_round_weight = exc, proj = p), expected)
})

test_that("Test plaus_weight_by_model_name: both, and some with weight 1",{
  p <- expand.grid(round = 1:3,
                   scenario_id = paste0(LETTERS[1:4], "-2020-02-01"),
                   model_name = c("A", "B")) %>%
    setDT()
  exc <- data.frame(model_name = c("A","B","A"),
                    round = c(1,1,2),
                    weight = c(0.5,0.5,0))
  expected <- p[, plaus_mod := ifelse(round == 1 & model_name %in% c("A", "B"), 0.5,
                                      ifelse(round == 2 & model_name == "A", 0, 1))]
  expect_equal(plaus_weight_by_model_name(modelname_round_weight = exc, proj = p), expected)
})


