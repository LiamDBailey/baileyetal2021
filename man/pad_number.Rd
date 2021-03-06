% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gen_tableS1.R
\name{pad_number}
\alias{pad_number}
\title{Prepare numbers for display in tables/plots}
\usage{
pad_number(x, digits = 3)
}
\arguments{
\item{x}{A number (double)}

\item{digits}{Number of significant figures to include}
}
\value{
Character string.
}
\description{
To prepare for submission. If numbers <0.001 round to 3 significant figures and use scientific notation.
If numbers are larger, round numbers to 3 significant figures and pad to right if needed (i.e. if rounding leads to 0s)
}
\examples{
#Small numbers (negative or positive) use scientific notation.
pad_number(-0.00000047641)
pad_number(0.00000047641)

#Larger numbers do not. Use 3 significant figures
pad_number(0.0128)

#Pad out to same length for all numbers
pad_number(0.1)
}
