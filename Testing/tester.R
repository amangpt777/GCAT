#  Analyze growth curve data and generate outputs and plots from an R session using GCAT package 

library(GCAT)

INPUT.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/B05.19 GCAT 4/3_GCAT_example_data_analysis"
INPUT.FILE = "YPDAFEXglucoseTests_2-25-10.csv"
OUTPUT.DIR = "/mnt/file.glbrc.org/ybukhman/Projects/GCAT_B05/B05.19 GCAT 4/3_GCAT_example_data_analysis/GCAT_4.4_debug/v467__single_plate_sigmoid"
start.index = 2

#  gcat.analysis.main expects to see this object in the environment from which it is called:
time.input = 1/3600 
#time.input = "%m/%d/%Y %H:%M"

setwd(INPUT.DIR)
out = gcat.analysis.main(file.list = INPUT.FILE, single.plate = T, layout.file = NULL,   
                         out.dir = OUTPUT.DIR, graphic.dir = OUTPUT.DIR, 
                         add.constant = 1, blank.value = NULL, start.index = start.index, growth.cutoff = 0.05,
                         use.linear.param=F, use.loess=F, smooth.param=0.1,
                         points.to.remove = 0, remove.jumps = F, 
                         silent = F, verbose = T, return.fit = T, overview.jpgs = T)
  

