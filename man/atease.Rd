\name{atease}
\alias{atease}
\alias{@}
\alias{@<-}
\title{S4-like syntax for accessing attributes of any object}
\description{Package \pkg{atease} lets you write \code{x@att} as shorthand for \code{attr( x, "att")}, even for non-S4 objects. \code{@<-} is the analogue for replacements, called automatically during e.g. \code{x@att <- 1}. For an S4 object, \code{@} and \code{@<-} access the slots, as usual; there might be some minuscule speed penalty, but (i) you shouldn't really be accessing slots of other people's S4 objects directly, and (ii) if you're writing your own S4 object, you won't be using the \pkg{atease} package! Although perfectly legal, this is no doubt considered all \emph{very naughty} and would be reviled by S4 fans...
}
\usage{
object@name
object@name<- value
}
\arguments{
\item{  object}{any object}
\item{  name}{the \emph{unquoted} name of an attribute (or slot)}
\item{  value}{what to put in the attribute (or slot)}
}
\examples{
l <- list(0)
attr( l[[1]], 'thing') <- 66
l[[1]]@thing # 66
l[[1]]@thing <- 44
attr( l[[1]], 'thing') # 44
}
\keyword{misc}
