\name{GCAT}
\alias{GCAT}
\title{
Growth Curve Analysis Tool
}
\description{
Mathematical modeling and parameter estimation of high volume microbial growth data.
}
\details{
GCAT utilizes the \code{\link{nls}} function in the R base package to fit logistic and Richards models to
growth curve data. Input is in .csv format and analysis is accessed using \code{\link{gcat.analysis.main}}
or \code{\link{gcat.fit.main}}. Output is in .txt and .pdf format, and is accessed using \code{\link{gcat.analysis.main}}
or \code{\link{gcat.output.main}}.

\tabular{ll}{
Version: \tab 5.0\cr
Depends: \tab pheatmap, gplots\cr
License: \tab LGPL-3\cr
Date: \tab 2014-02-10\cr
}

}
\author{
Jason Shao\cr
Nate DiPiazza\cr
Yury Bukhman\cr
Minh Bui\cr

Maintainer: Yury Bukhman
}