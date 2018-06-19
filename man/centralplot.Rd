\name{centralplot}
\alias{centralplot}
\title{
centralplot
}
\description{
Show the relationships between centre and peripheral items.
}
\usage{
centralplot(centrename, dataname, distance, bound, title = "centralplot", 
color = "#267ED6", size = 3.5, boundarycolor = "#EF715E", boundarysize = 1, 
centretextsize = 3, alpha = 0.5)
}
\arguments{
  \item{centrename}{
Name of the centre.
}
  \item{dataname}{
Name of peripheral items.
}
  \item{distance}{
Distance  between centre and peripheral items.
}
  \item{bound}{
A ceriterion to judge whether the relationship is close enough.
}
  \item{title}{
Title of the plot.
}
  \item{color}{
Color of peripheral items.
}
  \item{size}{
Size of peripheral items.
}
  \item{boundarycolor}{
Color of the boundary line.
}
  \item{boundarysize}{
Size of the boundary line.
}
  \item{centretextsize}{
Size of the text for the centre.
}
  \item{alpha}{
}
}
\author{
Jian Sun
}
\examples{
name <- c('a','b','c')
distance <- c(1,2,3)
bound <- 2.5
centralplot("centre",name,distance,bound)
}

