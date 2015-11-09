context("Blank Average Layout Testing for Exceptions")

INPUT.DIR =  system.file("extdata/default_examples",package="GCAT")
OUTPUT.DIR = paste(INPUT.DIR,"temp",sep="/")
INPUT.FILE = "single_YPDAFEXglucoseTests_2-25-10.csv"

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600


expect_that(gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                         out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                         add.constant = 1, blank.value = "average.layout", start.index = 2, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T), throws_error())