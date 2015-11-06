#  Test Issue7 Fix
#  GCAT normalize.ODs option to subtract an average of control wells at each time point for single plate

#  Initializing Variables

context("Blank Average Layout Testing")

INPUT.FILE = system.file("extdata/default_examples","single_YPDAFEXglucoseTests_2-25-10.csv",package="GCAT")
INPUT.LAYOUT = system.file("extdata/default_examples","single_YPDAFEXglucoseTests_2-25-10_Layout.csv",package="GCAT")
#OUTPUT.DIR = paste(getwd(),"GCAT_test_out_avg_layout_issue7",sep="/")
start.index = 1
time.input = 1/3600
normalize.method = "average.layout"
blank.value = NULL
add.constant = 0

library(GCAT)


plate.layout = read.csv(INPUT.LAYOUT,header=T,stringsAsFactors=F)

#  Load data and initializing well.array to test our normalize.ODs
well.array = GCAT:::gcat.load.data(file.name = INPUT.FILE, input.data = NULL, plate.layout = plate.layout,
                            plate.nrow = 8, plate.ncol = 12, input.skip.lines = 0,
                            multi.column.headers = c("Plate.ID", "Well", "OD", "Time"), single.column.headers = c("","A1"), 
                            layout.sheet.headers = c("Strain", "Media Definition"),
                         blank.value = blank.value, start.index = start.index,
                         silent = T,  single.plate = T, load.type = csv)

well.array = GCAT:::aapply(well.array, GCAT:::gcat.start.times, start.index = start.index, time.input = time.input)
well.array = GCAT:::aapply(well.array, GCAT:::remove.points, points = 0)

well.array = GCAT:::normalize.ODs(well.array, normalize.method = normalize.method, 
                               start.index = start.index, blank.value = blank.value, add.constant = add.constant)

#  Test output well.array for any issues
test_that("output well array is returned", {
  expect_equal(typeof(well.array), "list")
  expect_is(well.array, "array")
  expect_equal(length(well.array), 96)
  expect_true(all(sapply(well.array, is, "well")))
})

#  Test output well A1 norm field is a numeric list of length 135
test_that("output well array has norm as a list", {
  expect_is(well.array[[1,1,1]]@norm, "numeric")
  expect_equal(length(well.array[[1,1,1]]@norm), 136)
})

test_that("output average of 38th timestamp of well A1", {
  expect_equal(well.array[[1,1,1]]@norm[38], 0.08868409)
})



