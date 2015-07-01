#  Test that heatmaps are processed and generated correctly
#  Yury V Bukhman, 01 July 2015
#  This script generates heat maps with pre-set limits.  Manually check output directory to see if they make sense.

INPUT.DIR =  system.file("extdata/YPDAFEXglucoseTests_2-25-10",package="GCAT")
OUTPUT.DIR = paste(system.file("extdata/heatmap_tests",package="GCAT"),"temp",sep="/")
INPUT.FILE = "YPDAFEXglucoseTests_2-25-10.csv"

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600
out = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                         out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                         add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = integer(), remove.jumps = F, time.input=time.input,
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T,
                         lagRange = c(0,3), totalRange = c(0.2,0.82), specRange = c(0.04,0.13))

