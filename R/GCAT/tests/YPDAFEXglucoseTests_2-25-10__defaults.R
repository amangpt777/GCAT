#  Regression testing script for GCAT.
#  Yury V Bukhman, 17 Nov 2014
#  This script tests outputs generated from the example dataset "YPDAFEXglucoseTests_2-25-10" with default GCAT settings.

INPUT.DIR =  system.file("extdata/YPDAFEXglucoseTests_2-25-10",package="GCAT")
OUTPUT.DIR = paste(INPUT.DIR,"temp",sep="/")
INPUT.FILE = "YPDAFEXglucoseTests_2-25-10.csv"

#  Run GCAT
library(GCAT)
setwd(INPUT.DIR)
time.input=1/3600
out = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                         out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                         add.constant = 1, blank.value = NULL, start.index = 2, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = 0, remove.jumps = F, time.input=time.input,
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T)

#  Read in old and new output spreadsheets
old.output.table = read.table("default_output.txt",header=T,sep="\t")
new.output.file = list.files(path=OUTPUT.DIR, pattern = "^output_gcat\\.fit_.+\\.txt$")
new.output.table = read.table(paste("temp",new.output.file,sep="/"),header=T,sep="\t")

#  Remove "pdf.file" column
old.output.table = old.output.table[names(old.output.table) != "pdf.file"]
new.output.table = new.output.table[names(new.output.table) != "pdf.file"]

#  Verify that output spreadsheets are identical within rounding error
stopifnot(all.equal(new.output.table,old.output.table))
