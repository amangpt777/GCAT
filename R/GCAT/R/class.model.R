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
#  <model> class definition and functions. Objects contain equations   #
#  and other information for parameterized growth curve models.        #
#                                                                      #
########################################################################
setClass("model", representation(name = "character",
                                 expression = "expression",
                                 formula = "formula",
                                 guess = "function"))
# Slots:
#   name - a simple description of the model.
#   expression - an object of class "expression" that evaluates the response (transformed OD) with respect to the variable Time.
#   formula - same as expression, but with y as the response.
#   guess - a function that computes initial guesses for the parameters given a well object with a valid "screen.data" slot
#           containing useable OD values and slope estimates
# --------------------------------------------------------------------
###################### BEGIN PROTOTYPING ACCESSOR METHODS##############

# Minh: Let this code fragment be F1.
if (!isGeneric("getName")){
  if (is.function("getName"))
    fun <- getName
  else
    fun <- function(object) standardGeneric("getName")
    setGeneric("getName", fun)
}
# End of F1
setMethod("getName", "model", function(object) object@name)

# Minh: Let this line be F2.
setGeneric("getExpression", function(object){standardGeneric("getExpression")})
# Question: How is F1 different from F2?

setMethod("getExpression", "model",
          function(object){
            return(object@expression)
          })

setGeneric("getFormula", function(object){standeardGeneric("getFormula")})
setMethod("getFormula", "model", 
          function(object){
            return(object@formula)
          })

setGeneric("getGuess", function(object){standeardGeneric("getGuess")})
setMethod("getGuess", "model", 
          function(object){
            return(object@guess)
          })
######################## ENG PROTOTYPING ########################

# Function to create a new model
#' Model 
#' 
#' Function to create a new model 
#' @param name The name of the model 
#' @param expression Expression of the model 
#' @param formula The formula of this model 
#' @param guess The guess of this model 
#' @return The new model 
model = function(name, expression, formula, guess){
  new("model", name = name, expression = expression, formula = formula, guess = guess)
}

loess.g =  function(well,smooth.param=0.75){
  #data = data.from(well)
  #growth = data[,2]
  #Time = data[,1]
  Time = data.from(well)[,1]
  
  # predicted growth values to be used in estimating growth curve parameters
  loess.fit = loess(data.from(well)[,2]~Time,span=smooth.param)
  t = seq(from = min(Time), to = max(Time), by = (max(Time)-min(Time))/1000)
  y = predict(loess.fit, data.frame(Time=t))
  attr(y,"names") = NULL # need to remove the names to prevent them from showing up in the returned vector
  
  #  Remove any data points where y has not been estimated
  filt = is.finite(y)
  t = t[filt]
  y = y[filt] # remove any NA etc
  
  # specific growth using loess to find max derivative
  delta.t = diff(t)
  dydt = diff(y)/delta.t
  u = max(dydt)
  
  # lower and upper asymptotes
  b = min(y)
  A = max(y) - min(y)
  
  # inflection point
  inflection.pt.index = which.max(dydt)
  inflection.time = t[inflection.pt.index]
  inflection.y = y[inflection.pt.index]
  
  # lag time
  lam = inflection.time - (inflection.y-b)/u
  
  # Return named array of estimates
  c(A = A, b = b, lam = lam, u = u)
}


