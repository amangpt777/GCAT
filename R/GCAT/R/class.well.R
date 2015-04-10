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
#  <well> class definition and functions. Objects contain raw          #
#  data from screening runs on single wells from 96-well plates, and   #
#  other slots for processing and model-fitting details.               #
#                                                                      #
########################################################################

# Windows OS compatibility
Sys.setlocale(locale="C")
#require(RExcelXML)

#  Treat nls and loess as S4 classes to avoid warnings
setOldClass("nls")
setOldClass("loess")
setClass("well", representation(position = "character",
					  well.info = "list",
					  screen.data = "data.frame", 
            start.index = "numeric",
					  use.log = "logical",
					  norm = "numeric",
					  curve.par = "list", 
					  fit.par = "list",
            fit.std.err = "list",
					  equation = "expression",
					  model.name = "character",
					  fit.info = "character",
					  add.info = "character",
            inflection.time = "numeric",
            rss = "numeric",
            loess = "loess",
            nls = "nls"))

# Slots:
#   position - 3-member vector containing identifying information for the well: row (letters), column (numbers) and plate ID. 
#   well.info - a list containing strain and media names if provided
#   screen.data - a data frame with Time and raw OD values. This is the only slot that is filled upon creation of a well object. 
#                 as different functions are run on the well the data frame gets filled with additional columns. 
#   use.log - a single logical value denoting whether to return log-transformed values when data is requested from the well
#   norm - a value to subtract from all OD values before returning data. filled by <normalize.ODs> (see normalize.and.transform.R)
#   curve.par - a list of parameters that denote whether the well is empty, whether it contains ODs indicating a viable culture, whether it tanks at a certain timepoint.  

#   if model fitting using <fit.model> is successful:
#     fit.par - will be a list containing the fitted model parameters
#     fit.std.err - will be a list containing the standard errors for the fitted model parameters
#     equation - will contain an expression for evaluating the successfully fitted model 
#     model.name - will contain the name of the successfully fit model

#   fit.info - a message with info about whether the fit was successful, failed, or skipped. 
#   add.info - a message with info about whether jumps in OD were detected or removed, or if ODs were detected below the blank OD.
#   inflection.time - the Time value at the point where the specific growth is located. no longer a formula param NWD
#   rss - residual sum of squares
#   loess - object returned by running loess on the normalized well data
#   nls - object returned by running nls on the normalized well data

setGeneric("getPosition", function(object){standeardGeneric("getPosition")})
setMethod("getPosition", "well", 
          function(object){
            return(object@position)
          })

setGeneric("getWellInfo", function(object){standeardGeneric("getWellInfo")})
setMethod("getWellInfo", "well", 
          function(object){
            return(object@well.info)
          })

setGeneric("getScreenData", function(object){standeardGeneric("getScreenData")})
setMethod("getScreenData", "well", 
          function(object){
            return(object@screen.data)
          })

setGeneric("getStartIndex", function(object){standeardGeneric("getStartIndex")})
setMethod("getStartIndex", "well", 
          function(object){
            return(object@start.index)
          })

setGeneric("getUseLog", function(object){standeardGeneric("getUseLog")})
setMethod("getUseLog", "well", 
          function(object){
            return(object@use.log)
          })

setGeneric("getNorm", function(object){standeardGeneric("getNorm")})
setMethod("getNorm", "well", 
          function(object){
            return(object@norm)
          })

setGeneric("getCurPar", function(object){standeardGeneric("getCurPar")})
setMethod("getCurPar", "well", 
          function(object){
            return(object@curve.par)
          })

setGeneric("getFitErr", function(object){standeardGeneric("getFitErr")})
setMethod("getFitErr", "well", 
          function(object){
            return(object@fit.std.err)
          })

setGeneric("getEquation", function(object){standeardGeneric("getEquation")})
setMethod("getEquation", "well", 
          function(object){
            return(object@equation)
          })

setGeneric("getModelName", function(object){standeardGeneric("getModelName")})
setMethod("getModelName", "well", 
          function(object){
            return(object@model.name)
          })

setGeneric("getFitInfo", function(object){standeardGeneric("getFitInfo")})
setMethod("getFitInfo", "well", 
          function(object){
            return(object@fit.info)
          })

setGeneric("getAddInfo", function(object){standeardGeneric("getAddInfo")})
setMethod("getAddInfo", "well", 
          function(object){
            return(object@add.info)
          })

setGeneric("getInflectionTime", function(object){standeardGeneric("getInflectionTime")})
setMethod("getInflectionTime", "well", 
          function(object){
            return(object@inflection.time)
          })

setGeneric("getRSS", function(object){standeardGeneric("getRSS")})
setMethod("getRSS", "well", 
          function(object){
            return(object@rss)
          })

setGeneric("getLoess", function(object){standeardGeneric("getLoess")})
setMethod("getLoess", "well", 
          function(object){
            return(object@loess)
          })

setGeneric("getnls", function(object){standeardGeneric("getnls")})
setMethod("getnls", "well", 
          function(object){
            return(object@nls)
          })

setGeneric("getFitPar", function(object){standeardGeneric("getFitPar")})
setMethod("getFitPar", "well", 
          function(object){
            return(object@fit.par)
          })

# --------------------------------------------------------------------
# Function to create a new well (requires only Time and OD vectors, which will fill slot "screen.data")
# slots "nls" and "loess" are initialized to empty lists
well = function(Time = NULL, OD = NULL){
  x = list()
  class(x) = "loess"
  y = list()
  class(y) = "nls"
	new("well", screen.data = data.frame(Time, OD, stringsAsFactors=F), loess=x, nls=y)
}

# -----------------------------------------------------------------------
#### A show method for well  ####
setMethod("show", "well",
          function(object) {
            print("Object of class well")
            print("@position:")
            print(object@position)
            print("@well.info:")
            print(object@well.info)
            print("@screen.data:")
            print(head(object@screen.data))
            print("...")
            print(paste(nrow(object@screen.data),"rows of data"))
            print(paste("@start.index:",object@start.index))
            print(paste("@use.log:",object@use.log))
            print(paste("@norm:",object@norm))
            print("@curve.par:")
            print(object@curve.par)
            print("@fit.par:")
            print(object@fit.par)
            print("@fit.std.err:")
            print(object@fit.std.err)
            print(paste("@equation:",object@equation))
            print(paste("@model.name:",object@model.name))
            print(paste("@fit.info:",object@fit.info))
            print(paste("@add.info:",object@add.info))
            print(paste("@inflection.time:",object@inflection.time))
            print(paste("@rss:",object@rss))
            if (length(object@nls) > 0) {
              print("@nls:")
              print(object@nls)
            } else {
              print("no nls model")
            }
            if (length(object@loess) > 0) {
              print("@loess:")
              print(object@loess)
            } else {
              print("no loess model")
            }
          }
          )

#### A plot method for well  ####
#  x - object of class well
#  y - not used
#  constant.added - used to readjust for the constant added during the log transform: log.OD = log(OD - blank + constant.added)
#  xlim - x axis limits, vector of length 2
#  ylim - y axis limits, vector of length 2
#  scale - determines the font scale for the entire graph. all cex values are calculated from this
#  number.points - should points be labeled with numeric indices?
#  draw.symbols - should <check.slopes> be called on the well and markings drawn on the graph?
#  show.text - show R^2 and growth curve parameters as text on the plot
#  show.calc - draw lines that illustrate growth curve parameters
#  draw.guess - initial guess model.  Drawn if specified
#  well.number - the number of the well in an array of wells
#  ... - additional arguments passed to the generic plot function

setMethod("plot",
          signature(x = "well", y="missing"),
          function (x, y, constant.added = 1.0, xlim = NULL, ylim = NULL,
                    well.number = NULL, scale = 1, number.points = T, draw.symbols = F, show.text = T, show.calc = T, draw.guess = NULL, ...) 
          {
            # Determine the boundaries for the axes (if user did not specify them)
            if(is.null(ylim)){
              min.y = min(data.from(x, remove = F, remove.tanking = F)[,2], na.rm = T)
              min.y = min(min.y, x@fit.par$b)
              max.y = max(data.from(x, remove = F, remove.tanking = F)[,2], na.rm = T)
              max.y = max(max.y, x@fit.par$b + x@fit.par$A)
              ylim = c(min.y, min.y + (max.y-min.y)*1.15)
            }
            if(is.null(xlim)){
              min.x = min(data.from(x, remove = F, remove.tanking = F)[,1], na.rm = T)
              max.x = max(data.from(x, remove = F, remove.tanking = F)[,1], na.rm = T)
              xlim = c(min.x - 0.05 * (max.x-min.x), max.x)
            }
            
            
            # Title of plot: [well number] plate name; well name;
            #                strain name; media name
            
            main = paste(plate.name(x), " ", well.name(x), "\n",
                         strain.name(x), "; ", media.name(x), sep = "")
            if (!is.null(well.number)) main = paste("[", well.number , "] ", main, sep="")
            
            # Draw the data and symbols if <draw.symbols> is true.
            plot.data(x, main = main, scale = scale, constant.added=constant.added, 
                      number.points = number.points, draw.symbols = draw.symbols, xlim = xlim, ylim = ylim, ...)
            
            # Draw the fitted model.
            plot.model(x, scale = scale, constant.added=constant.added)
            
            # Draw text info if specified. 
            if(show.text)
              draw.text(x, scale = scale * 0.5, xlim = xlim, ylim = ylim,...)
            
            # Show calculated parameters if specified. 
            if (show.calc)
              draw.calc.par(x, scale = scale * 0.5, constant.added = constant.added)
            
            # Draw initial guess if a model is specified. 
            if (class(draw.guess) == "model"){
              Time = data.from(x)$Time
              guess = eval(getExpression(draw.guess), as.list(getGuess(draw.guess)(x)))
              try(lines(Time, guess, col = "brown2"), silent = T)
            }
          }
)

########################################################################
# Some miscellaneous functions to extract info from well objects       #
# Most of these return a single value from the well.                   #
########################################################################
#
#   Since many of these need to be applied to all wells over an array, while conserving the dimensions of 
#   that array, this file includes a wrapper function <aapply> (see bottom of file).

plate.name = function(well)
	getPosition(well)[1]

# Return the full alphanumeric well name (with leading zeros if applicable)
well.name = function(well){
	row = getPosition(well)[2]
	col = as.numeric(getPosition(well)[3])
	if (col>9)
		col = as.character(col)
	else
		col = paste("0", col, sep = "")

	paste(row,col,sep = "")
	}

is.empty = function(well)
	getCurPar(well)$empty.well

lacks.growth = function(well)
	getCurPar(well)$no.growth

tanking.start = function(well)
	getCurPar(well)$tanking.start

removed.points = function(well)
	(1:length(well))[getScreenData(well)$Remove]

remaining.points = function(well,...){
	as.numeric(rownames(data.from(well,...)))
	}

strain.name = function(well){
  if(is.null(getWellInfo(well)$Strain))
    return("<NA>")
  else
    return(getWellInfo(well)$Strain)
  }
media.name = function(well){
  if(is.null(getWellInfo(well)$Media))
    return("<NA>")
  else
    return(getWellInfo(well)$Media)
  }

raw.data = function(well)
	data.from(well, remove.tanking = F, remove = F, na.rm = F, raw.data = T)

contains.fit = function(well)
	length(getFitPar(well)) > 0

setMethod("length", signature(x = "well"), function(x) length(x@screen.data[,1]))

#   The <data.from> function has some options: by default it returns a two-column data frame with time and OD 
#   (or log OD if the <use.log> slot is true in the object), after normalization to the value specified in <norm> slot.    
#   - With <remove> set to true the rows specified in the <remove> column of the <screen.data> slot are not returned. 
#   - With <remove.tanking> set to true all the rows after the <tanking.start> index are removed. 
#   - Setting <raw.data> to true overrides all these settings and just returns 2 columns with Time and Raw OD.

data.from = function(well, remove = T, remove.tanking = T, raw.data = F, na.rm = F){
	
	if (length(getUseLog(well)) == 0)
		OD.column = "OD"
	else if (getUseLog(well))
		OD.column = "log.OD"
	else
		OD.column = "OD"
	
	if (raw.data){
		OD.column = "OD"
		norm = 0
		}
	else if (!getUseLog(well))
		norm = getNorm(well)
	else
		norm = 0

	if(remove.tanking & is.numeric(tanking.start(well)))
		well = remove.points(well, (tanking.start(well)):length(well))
	if (!remove | is.null(getScreenData(well)$Remove))
		output = getScreenData(well)[c("Time", OD.column)]
	else
		output = getScreenData(well)[!getScreenData(well)$Remove ,c("Time", OD.column)]

	output[,2] = output[,2] - norm

	if (!raw.data){
		if (!length(getUseLog(well)))
			names(output)[2] = "Corrected.OD"
		if (!getUseLog(well))
			names(output)[2] = "Corrected.OD"
		}

	if (na.rm)
		output[!is.na(output[,2]),]	 
	else
		output
	}


# Functions much like <data.from> but gives a single vector containing the 
# slope at each point. Has a parameter allowing removal of NA values. 

slopes = function(well, remove = T, remove.tanking = T, na.rm = F){

	if(remove.tanking & is.numeric(tanking.start(well)))
		well = remove.points(well, (tanking.start(well)):length(well))
	if (!remove | is.null(getScreenData(well)$Remove))
		output = getScreenData(well)$Slope
	else
		output = getScreenData(well)$Slope[!getScreenData(well)$Remove]

	if (na.rm)
		output[!is.na(output)]	 
	else
		output
	}

# -----------------------------------------------------------------------
# Well array functions: these must be used on entire arrays of well objects
# instead of single ones. 

plate.names = function(well.array)
	dimnames(well.array)[[3]]

tanking.start.values = function(well.array, array = F){
	if (array)
		aapply(well.array, function(well) tanking.start(well))
	else
		sapply(well.array, function(well) tanking.start(well))
	}

