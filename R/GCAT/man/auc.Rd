% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/fitted.calculations.R
\name{auc}
\alias{auc}
\alias{auc.OD}
\title{Compute Area Under The Curve (AUC)}
\usage{
auc(fitted.well, start = NULL, end = NULL, digits = 3, unlog = FALSE,
  constant.added = 0)

auc.OD(fitted.well, start = NULL, end = NULL, digits = 3,
  constant.added = 0)
}
\arguments{
\item{fitted.well}{object of class \linkS4class{well} with fitted model info}

\item{start}{starting time for AUC calculation}

\item{end}{end time for AUC calculation}

\item{digits}{number of significant digits to output}

\item{unlog}{logic, if TRUE, compute AUC on linear OD scale}

\item{constant.added}{constant added in the \emph{log.OD = log(OD - blank + const)} transformation}
}
\value{
Returns numeric AUC value computed using the well's \emph{nls} or \emph{loess} model.
  Returns NA if neither \emph{nls} nor \emph{loess} model is defined for the well.
}
\description{
Compute Area Under The Curve (AUC)
}
\section{Functions}{
\itemize{
\item \code{auc.OD}: Compute Area Under The Curve (AUC) on linear OD scale
}}
\seealso{
\linkS4class{well}
}

