% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tnorm_pdf.R
\name{tnorm_pdf}
\alias{tnorm_pdf}
\title{Calculate the PDF of a Normal Truncated Above}
\usage{
tnorm_pdf(x, mu, sigma, k, ub)
}
\arguments{
\item{x}{The point where we assess the PDF.}

\item{mu}{The distribution's mean.}

\item{sigma}{The distribution's SD.}

\item{k}{The coefficient on the signal standard variance.}

\item{ub}{The upper truncation of the normal.}
}
\value{
The PDF at \code{x}
}
\description{
Calculate the PDF of a Normal Truncated Above
}
