% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Table1_functions.R
\name{final_table}
\alias{final_table}
\title{A function to create a summary table for 1 or more groups.}
\usage{
final_table(data, variables, group, margin = 2, single = F, ron,
  col.names = T, summary.stat = "both")
}
\arguments{
\item{data}{a data frame containing the variables listed in variables 
and the group variable.}

\item{variables}{a vector or scalar of variables of interest, in quotation marks.}

\item{group}{an R object. Grouping variable can have >=1 level(s).}

\item{margin}{a number, 1 or 2, giving the desired margin to calculate percentages over.
1 indicates rows, 2 indicates columns.}

\item{single}{a logical indicating whether categorical variables with 2 levels should be
presented as a single row. This row will correspond to the second level listed for the factor.}

\item{ron}{a number giving the desired decimal places in rounding for continuous variables. 
Defaults to 0 unless otherwise indicated.
Note: p value rounding set to 4 digits.}

\item{col.names}{a logical value indicating whether the column labels with group name and sample size are desired.}

\item{summary.stat}{a character string specifying the summary statistic for continuous variables, 
must be one of "both" (default), "median" or "mean".}
}
\description{
A function to create a summary table for 1 or more groups.
}
\details{
Can handle >=1 group; number of columns will adjust automatically. T-test or ANOVA performed 
for continuous variables with skewness <=3. Chi-squared or Fisher's Exact (for expected cell value <5) performed 
for categorical variables. Fisher's exact test is indicated by * next to variable name. One summary column in the case of a single group.
}
\examples{
iris$sepal_di<-as.factor((iris$Sepal.Length<5)*1)
#set factor levels
levels(iris$sepal_di)=c("<5",'>=5')
levels(iris$Species)=c('Setosa','Versicolor','Virginica')
#Set Labels
label(iris$sepal_di)='Sepal Length'
label(iris$Sepal.Length)="Sepal Length"
label(iris$Sepal.Width)='Sepal Width'
label(iris$Petal.Length)='Petal Length'
label(iris$Petal.Width)='Petal Width'
tab1<-final_table(iris,c('Sepal.Length','Petal.Length','Sepal.Width','Petal.Width','sepal_di'),iris$Species,1,T,2)
}
\keyword{1}
\keyword{Table}
