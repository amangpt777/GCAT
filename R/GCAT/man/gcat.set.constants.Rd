\name{gcat.set.constants}
\alias{gcat.set.constants}
\title{
Set global constants for GCAT analysis package
}
\description{
Sets global constants, mostly regarding issues in input file format, for GCAT analysis. 
}
\usage{
gcat.set.constants(plate.nrow = 8, plate.ncol = 12, input.skip.lines = 0, time.format = "\%Y-\%m-\%d \%H:\%M:\%S", 
multi.column.headers = c("Plate ID", "Well", "OD", "Time"), single.column.headers = c("", "A1"), 
xlsx.data.headers = c("Plate ID", "Well positions"), xlsx.layout.sheet = "Plate layout", 
layout.sheet.headers = c("Strain", "Media Definition"))
}
\arguments{
  \item{plate.nrow}{
Number of rows present in each plate of input data. Default 8 (A-H)
}  
  \item{plate.ncol}{
Number of columns present in each plate of input data. Default 12 (1-12)
}  
  \item{input.skip.lines}{
Number of lines to skip at the top when reading input data files. 
}
  \item{time.format}{
Either a character describing the format used to convert timestamps 
in the input to numbers representing number of seconds (see \code{\link{strptime}}), or a 
factor to divide entries in the \code{Time} column by to get the number of hours. 
}  
  \item{multi.column.headers}{
A character vector describing the names of the columns for 
Plate ID, Well ID, Cellular density measurements and Time, respectively, in the multi-plate (long) format.
}
  \item{single.column.headers}{
A character vector describing the name of the Time column and the first well data in the single plate (wide) format. 
}
  \item{xlsx.data.headers}{
For .xlsx data only, a vector describing possible entries in the upper left cell marking worksheets in each
workbook as containing data. .csv files don't have multiple worksheets and are assumed to contain useable data.
}
  \item{xlsx.layout.sheet}{
For .xlsx data only, the name of the worksheet containing plate layout information. .csv files use a separate layout file. 
}
  \item{layout.sheet.headers}{
A character vector describing the name of the Strain and Media definiton columns, respectively, in the plate layout file. 
}
}
\value{
NULL
}
\author{
Jason Shao
}