#  Test AUC computation

library(GCAT)
load(system.file("extdata/YPDAFEXglucoseTests_2-25-10","default_output.RData",package="GCAT"))

#  Compute upper bound of the AUC
well = out[[30]]
screen.data = getScreenData(well)
constant.added = screen.data$OD[1] - getNorm(well)
upper.bound = achieved.growth(well)*max(screen.data$Time)
upper.bound.OD = achieved.growth.OD(well,constant.added)*max(getScreenData(well)$Time)

#  Compute the AUC and check that it is in the right range
auc = auc(well)
auc.OD = auc.OD(well,constant.added)
test_that("auc is in the right range", {
  expect_less_than(auc, upper.bound)
  expect_less_than(auc.OD, upper.bound.OD)
  expect_more_than(auc, upper.bound/2)
  expect_more_than(auc.OD, upper.bound.OD/2)
})

#test