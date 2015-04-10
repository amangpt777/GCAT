#!/usr/bin/env Rscript

#  Compare 2 versions of GCAT for a particular parameter
#  Karan Maini

  args <- commandArgs(TRUE) 

    # Baaad....WORK.DIR = "/home/maini/Desktop/GCAT Test Script"

	# INPUT.1 = "GCAT2/output_gcat.fit_2013-11-18_12.23.34.txt"
	# INPUT.2 = "GCAT3/output_gcat.fit_2013-11-18_12.19.46.txt"
	
	#  Read in the data
	in1 = read.table(args[1],header=T,sep="\t")
	in2 = read.table(args[2],header=T,sep="\t")
	parameter = args[3]
	
	#  Compare values
	same = signif(in1$spec.growth,2) == signif(in2$spec.growth,2) & signif(in1$tot.growth,2) == signif(in2$tot.growth,2) &  signif(in1$lag.time,2) == signif(in2$lag.time,2)
	same[is.na(in1$spec.growth) & is.na(in2$spec.growth)] = T
	
  #  Write out the output
	#out = file(OUTPUT,open="w")
	#if (all(same)) { writeLines("No significant differences identified",con=out) }
	#close(out)
	
	#  Manual exploration
	#  Ascertain that results are quite different
	
	cat("Command:\nRscript compare_gcat_output.R",args[1],args[2],parameter)
	
	cat("\n\nSummary: \n\n")
	summary(same)
	head(same)
	cat("\n\nGCAT2: \n\n")
	head(in1)
	cat("\n\nGCAT3: \n\n")
	head(in2)
	cat("\n\nSpecific Growth Comparison Summary: \n\n")
	summary(in2$spec.growth/in1$spec.growth) # specific growth lower in GCAT 3
	cat("\n\nTotal Growth Comparison Summary: \n\n")
	summary(in2$tot.growth/in1$tot.growth)
	cat("\n\nLag Time Comparison Summary: \n\n")
	summary(in2$lag.time/in1$lag.time)  #  lag time also lower in GCAT 3
	
	
	# Baaad....WORK.DIR = "/home/maini/Desktop/GCAT Test Script/Comparison Plots"
	setwd("./Comparison Plots")
  
  pdf( paste( parameter,'Plot.pdf'),onefile=T)
  #   ....plot code here....

#  Plot specific growth rate of GCAT 3 vs. GCAT 2
	x = in1$spec.growth
	y = in2$spec.growth
  
  plot(x,y,pch=16,main="Specific Growth", xlab="old GCAT", ylab="new GCAT")
  abline(0,1)
  abline(lm(y~x),lty=2)
  legend("bottomright",c("y=x line","regression line"),lty=c(1,2))

#  Plot total growth rate of GCAT 3 vs. GCAT 2
	x = in1$tot.growth
	y = in2$tot.growth
  
  plot(x,y,pch=16,main="Total Growth", xlab="old GCAT", ylab="new GCAT")
  abline(0,1)
  abline(lm(y~x),lty=2)
  legend("bottomright",c("y=x line","regression line"),lty=c(1,2))

#  Plot lag time of GCAT 3 vs. GCAT 2
	x = in1$lag.time
	y = in2$lag.time
  
  plot(x,y,pch=16,main="Lag Time", xlab="old GCAT", ylab="new GCAT")
  abline(0,1)
  abline(lm(y~x),lty=2)
  legend("bottomright",c("y=x line","regression line"),lty=c(1,2))

#  Plot Goodness of Fit of GCAT 3 vs. GCAT 2
	x = in1$good.fit
	y = in2$good.fit
  
  plot(x,y,pch=16,main="Goodness of Fit", xlab="old GCAT", ylab="new GCAT")
  abline(0,1)
  abline(lm(y~x),lty=2)
  legend("bottomright",c("y=x line","regression line"),lty=c(1,2))
  
  dev.off()
 
  
	
	
	#  Investigate outliers
	subset(in1, is.finite(in1$spec.growth) & (in1$spec.growth < in2$spec.growth - 0.003))
	subset(in2, is.finite(in1$spec.growth) & (in1$spec.growth < in2$spec.growth - 0.003))
	
	#  Clean up
	rm(list=ls())
    setwd("../") #move back to testing root directory