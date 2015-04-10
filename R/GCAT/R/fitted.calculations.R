#Copyright 2012 The Board of Regents of the University of Wisconsin System.
#Contributors: Jason Shao, James McCurdy, Enhai Xie, Adam G.W. Halstead, 
#Michael H. Whitney, Nathan DiPiazza, Trey K. Sato and Yury V. Bukhman
#
#This file is part of GCAT.
#
#GCAT is free software: you can redistribute it and/or modify
#it under the terms of the GNU Lesser General Public License as published by
#the Free Software Foundation, either version 3 of the License, or
#(at your option) any later version.
#
#GCAT is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#GNU Lesser General Public License for more details.
#
#You should have received a copy of the GNU Lesser General Public License  
#along with GCAT.  If not, see <http://www.gnu.org/licenses/>.

########################################################################
#                                                                      #
# Functions to calculate various things about wells based on fit model #
#                                                                      #
########################################################################

# S3 generic
lag <- function(fitted.well, ...)
{
  UseMethod("lag")
}

#
#  Common arguments:
#   fitted.well - should be a well containing the results of <fit.model>, most functions will return NA if well has not been fit yet.
#   unlog - should the value be returned on the linear scale as opposed to the log-transformed scale?
#   constant.added - for returning values on the linear scale, what was the constant added before the log transform?
#   digits - passed to the <round> function, default is no rounding (infinity digits)

unlog = function(x, constant.added) {
  ########################################################################
  #   Transform values back to OD scale                                  #
  ########################################################################
exp(x) - constant.added
}

well.eval = function(fitted.well, Time = NULL){
  ########################################################################
  #   Evaluate estimated OD at any timepoints using the fitted model     #
  ########################################################################

  # If no timepoints are provided, use the ones collected in the experiment itself.
	if(!is.numeric(Time))
		Time = data.from(fitted.well)$Time

  #  Use of equation is deprecated.  Use nls and loess models stored in the well object instead
  # Attempt to use <eval> with the fitted equation and parameters to get estimates for OD at the given timepoints.
	#output = try(eval(fitted.well@equation, fitted.well@fit.par), silent = T)
  
  #  Predict log.OD value(s) using nls model if present.  If no nls model, try using loess.
  if (length(fitted.well@nls)>0) {
    output = try(predict(fitted.well@nls,list(Time=Time)),silent=T)
  } else if (length(fitted.well@loess)>0) {
    output = try(predict(fitted.well@loess,Time),silent=T)
  } else {
    output = NA
  }

  # Return values. If OD evaluation failed for any reason, return NULL.
  if (is.numeric(output)){
    return(output)
  } else {
    return(NULL)
	}
}

model.residuals = function(fitted.well, unlog = F){
  ########################################################################
  #   Evaluate model residuals using the measured vs. fitted log.OD values   #
  ########################################################################
	measured.OD = data.from(fitted.well)[,2]

	# Use <well.eval> with no Time argument to get fitted OD values at measured timepoints.
	predicted.OD = well.eval(fitted.well)

	# If all values are valid, return the differences
	if (!is.numeric(predicted.OD))
		return(NA)
	else
    return(measured.OD - predicted.OD)
	}

dev.from.mean = function(fitted.well){
  ########################################################################
  #   Evaluate deviations of log.OD values from the mean                 #
  ########################################################################
  measured.ODs = data.from(fitted.well,remove=T,na.rm=T)[,2]
  
  # Get the mean values of these measured ODs.
  mean.ODs = mean(measured.ODs)
  
  if (!is.numeric(mean.ODs))
    return (NA)
  else
    return (measured.ODs - mean.ODs)
}

rss = function(fitted.well){
  #######################################################################
  #  Get the residual sum of square.                                    #
  #######################################################################
  if (length(fitted.well@rss) == 0)
    return (NA)
  else
    return (fitted.well@rss)
}

model.good.fit = function(fitted.well, digits = Inf){
  ########################################################################
  #   Calculate a metric for fit accuracy using squared residuals        #
  ########################################################################

  # Sum of squared residuals
	RSS = rss(fitted.well)
  
  # Total sum of squared
  tot = sum(dev.from.mean(fitted.well)^2)
  
  #  Coefficient of determination
  return (1 - RSS/tot)
	}

parameter.text = function(fitted.well){
  ########################################################################
  #           Output a string with values of fitted parameters           #
  ########################################################################
  
  # Get a list of fitted parameters
  fit.par = fitted.well@fit.par
  
  # Giving the parameter text descriptive names.
  if (length(fitted.well@fit.par) != 0){
    names(fit.par)[1] = "A" 
    names(fit.par)[2] = "b" 
    names(fit.par)[3] = "lambda" 
    names(fit.par)[4] = "max.spec.growth.rate" 
   
    if (fitted.well@model.name == "richards sigmoid"){ 
      names(fit.par)[5] = "shape.par" 
    } 
    
    if (fitted.well@model.name == "richards sigmoid with linear par."){ 
      names(fit.par)[5] = "shape.param" 
      names(fit.par)[6] =  "linear term"
      } 
  
    if (fitted.well@model.name == "logistic sigmoid with linear par.")
      names(fit.par)[5] = "linear.term"
  
    # if loess, just show smoothing param
    if(fitted.well@model.name == "local polynomial regression fit.")
      fit.par = fitted.well@fit.par["smoothing parameter"]
  }
 
  # Return nothing if the list is empty. Otherwise, concatenate the terms in the list with the parameter names.
	if(!is.list(fit.par))
		return()
  else{
  	output = ""
  	i = 1
  	while(i <= length(fit.par)){
  		output = paste(output, names(fit.par)[i], "=", round(as.numeric(fit.par[i]),3), "; ", sep = "")
  		i = i + 1
      if (i %% 6 == 0)
        output = paste(output, "\n")
  		}
  	output
  	}
	}

max.spec.growth.rate = function(fitted.well, digits = Inf, ...){
  ########################################################################
  #        Calculate maximum specific growth rate                       #
  ########################################################################
  if(length(fitted.well@fit.par) == 0)
    return(NA)
  
  round(fitted.well@fit.par$u,digits)
}


plateau = function(fitted.well, digits = Inf){
  ########################################################################
  #        Calculate plateau log.OD from fitted parameters               #
  ########################################################################
  if(length(fitted.well@fit.par) == 0)
    return(NA)
  
  plat = fitted.well@fit.par$A + fitted.well@fit.par$b
  
	if (!is.numeric(plat)) {
	  plat = NA
	} else {
    plat = round(plat, digits)
	}
	return(plat)
}

baseline = function(fitted.well, digits = Inf){
  ########################################################################
  #        Calculate baseline log.OD from fitted parameters              #
  ########################################################################
  if(length(fitted.well@fit.par) == 0)
    return(NA)

  base = fitted.well@fit.par$b

  # If A (plateau OD) is invalid, return NA.
	if (!is.numeric(fitted.well@fit.par$A))
		base = NA
  # If b (baseline OD) is invalid but plateau OD was valid, return zero.
  else if (!is.numeric(base))
		base = 0
	else{
		  base = round(base, digits)
		}
	return(base)
	}

inoc.log.OD = function(fitted.well, digits = Inf){
  ########################################################################
  #        Calculate log.OD at inoculation from fitted parameters        #
  ########################################################################

  # Evaluated the fitted model at the inoculation timepoint (should be zero from using <start.times> from table2wells.R)
	if (is.null(well.eval(fitted.well)))
		return(NA)
  else{
    inoc.time = fitted.well@screen.data$Time[fitted.well@start.index]
    inoc.log.OD = well.eval(fitted.well, inoc.time)
    if (is.na(inoc.log.OD)) inoc.log.OD = fitted.well@fit.par$b # need this in a special case: loess fits with start.index = 1 
    return(round(inoc.log.OD, digits))
    }
	}

max.log.OD = function(fitted.well, digits = Inf, ...){
  ########################################################################
  #        Calculate max log.OD from model fit                           #
  ########################################################################

  # Evaluated the fitted model at the final timepoint (just the last valid timepoint in the experiment)
	if (is.null(well.eval(fitted.well)))
		return(NA)
  else{
  	return(round(max(well.eval(fitted.well),na.rm=T), digits))
  }
}


projected.growth = function(fitted.well,digits=Inf) {
  ########################################################################
  #   Calculate projected growth: plateau minus the inoculated log.OD    #
  ########################################################################
	plateau(fitted.well,digits) - inoc.log.OD(fitted.well,digits)
}

projected.growth.OD = function(fitted.well,constant.added,digits=Inf) {
  ########################################################################
  #   Calculate projected growth: plateau minus the inoculated log.OD    #
  ########################################################################
  value = unlog(plateau(fitted.well),constant.added) - unlog(inoc.log.OD(fitted.well),constant.added)
  round(value,digits)
}


achieved.growth = function(fitted.well,digits=Inf) {
  ########################################################################
  #   Calculate achieved growth: max.log.OD minus the inoculated log.OD  #
  ########################################################################
  max.log.OD(fitted.well,digits) - inoc.log.OD(fitted.well,digits)
}

achieved.growth.OD = function(fitted.well,constant.added,digits=Inf) {
  ########################################################################
  #   Calculate projected growth: plateau minus the inoculated log.OD    #
  ########################################################################
  value = unlog(max.log.OD(fitted.well),constant.added) - unlog(inoc.log.OD(fitted.well),constant.added)
  round(value,digits)
}

reach.plateau = function(fitted.well, cutoff = 0.75){
  ########################################################################
  # Did the curve come close to the plateau OD during the experiment?    #
  ########################################################################

  plat = plateau(fitted.well)
  inoc = inoc.log.OD(fitted.well)
  final = max.log.OD(fitted.well)

	if (!is.na(final)){
    # If the plateau is the same as the OD at inoculation, return TRUE
    if ((plat - inoc) == 0)
      return(T)
     # If the difference between the final OD and inoculation OD is at least a certain proportion
     #  <cutoff> of the difference between the plateau and inoculated ODs, return TRUE.
    else
      return((final - inoc) / (plat - inoc) > cutoff)
		}
	else
		return(T)
		# If no final OD was calculated (if curve was not fit properly) just return T.
	}


lag.time = function(fitted.well, digits = Inf, ...){
  ########################################################################
  #              Calculate the lag time from the fitted OD               #
  ########################################################################
  if(length(fitted.well@fit.par) == 0)
    return(NA)
  
  fitted.well@fit.par$lam
}

# new params for GCAT 4.0

amplitude = function(fitted.well){
  if(length(fitted.well@fit.par) == 0)
    return(NA)
  
  return(fitted.well@fit.par$A)
}

shape.par = function(fitted.well){
    if(length(fitted.well@fit.par) == 0)
    return(NA)
  ifelse(is.null(fitted.well@fit.par$v), NA, fitted.well@fit.par$v)
}

max.spec.growth.rate.SE = function(fitted.well, ...){
  if(length(fitted.well@fit.std.err) == 0)
    return(NA)
  ifelse(is.null(fitted.well@fit.std.err$u), NA, fitted.well@fit.std.err$u)
}

lag.time.SE = function(fitted.well, ...){
  if(length(fitted.well@fit.std.err) == 0)
    return(NA)
  ifelse(is.null(fitted.well@fit.std.err$lam), NA, fitted.well@fit.std.err$lam)
}

shape.par.SE = function(fitted.well){
  if(length(fitted.well@fit.std.err) == 0)
    return(NA)
  ifelse(is.null(fitted.well@fit.std.err$v), NA, fitted.well@fit.std.err$v)
}

amplitude.SE = function(fitted.well){
  if(length(fitted.well@fit.std.err) == 0)
    return(NA)
  ifelse(is.null(fitted.well@fit.std.err$A), NA, fitted.well@fit.std.err$A)
}

baseline.SE = function(fitted.well){
  if(length(fitted.well@fit.std.err) == 0)
    return(NA)
  ifelse(is.null(fitted.well@fit.std.err$b), NA, fitted.well@fit.std.err$b)
}

# used to calulate the inflection.time value
inflection.time = function(well){
  if (length(well@loess) == 0 && length(well@nls) == 0) return(NA) # can' compute inflection time in the absence of a fit
  data = data.from(well)
  Time = data[,1]
  t = seq(from = min(Time), to = max(Time), by = (max(Time)-min(Time))/1000)
  y = well.eval(well,t)
  if (is.null(y)) return(NA)
  delta.t = diff(t)
  dydt = diff(y)/delta.t
  infl.index = which.max(dydt)
  t[infl.index]
}
