% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_misc.R
\name{addNCountColumn}
\alias{addNCountColumn}
\title{Add a column to a tidy data set with the count of observations appended}
\usage{
addNCountColumn(dataSet, countField, addNewline = T)
}
\arguments{
\item{addNewline}{Optional: Add a newline between the category name and the variable count, defaults to true}

\item{dataSet:}{A tidy data set}

\item{countField:}{A column to use for counting}
}
\description{
This function returns a tidy data set with one addition column which contains
a variable name with the counts appended. As an example, suppose you had 10 
observations of category (column named your_category) 'A' and 12 observations
of category 'B'. The output will add this additional column:
your_category.
<chr> 
A\\n(n=10) 
B\\n(n=12)
}
\details{
This is most useful for making a new categorical variable name to pass to a 
ggplot bar/box plot.
}
