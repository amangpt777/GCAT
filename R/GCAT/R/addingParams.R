setwd("~/Downloads/")
file.list = file.name = "YPDAFEXglucoseTests_2-25-10.csv"
layout.file = "YPDAFEXglucoseTests_2-25-10_Layout.csv"
single.plate = T
out.dir = getwd()
graphic.dir = paste(out.dir, "/pics", sep = "")
add.constant = 1
blank.value = NULL
start.index = 2
growth.cutoff = 0.05
use.linear.param = F
use.loess = F
smooth.param = 0.6
points.to.remove = 0
remove.jumps = F
silent = F
verbose = T
return.fit = F
overview.jpgs = T
plate.nrow = 8
plate.ncol = 12
input.skip.lines = 0
multi.column.headers = c("Plate ID", "Well", "OD", "Time")
single.column.headers = c("","A1")
layout.sheet.headers = c("Strain", "Media Definition")

t = gcat.analysis.main(file.list, single.plate, layout.file = NULL,   
                   out.dir = getwd(), graphic.dir = paste(out.dir, "/pics", sep = ""), 
                   add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                   use.linear.param=use.linear.param, use.loess=use.loess, smooth.param=0.1,
                   points.to.remove = 0, remove.jumps = F, time.input = NA,
                   plate.nrow = 8, plate.ncol = 12, input.skip.lines = 0,
                   multi.column.headers = c("Plate.ID", "Well", "OD", "Time"), single.column.headers = c("","A1"), 
                   layout.sheet.headers = c("Strain", "Media Definition"),
                   silent = F, verbose = F, return.fit = F, overview.jpgs = T)
