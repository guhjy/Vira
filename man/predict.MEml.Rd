% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/predict.MEml.R
\name{predict.MEml}
\alias{predict.MEml}
\alias{predict.MECTree}
\alias{mkNewReTrms}
\alias{mkNewReTrmsV2}
\alias{predict.MEglmTree}
\alias{predict.MEgbm}
\alias{predict.MEgbmRules}
\alias{predict.MErfRules}
\alias{predict.MEmixgbm}
\title{Predictions for MEml  
Make predictions using fitted MEml models: MEgbm, MEgbmrules, MErfrules, MEglmtree, MECTree, etc.}
\usage{
predict.MECTree(object, newdata, type = c("prob", "class")[1], ...)

mkNewReTrms(object, newdata, re.form = NULL, allow.new.levels = FALSE)

mkNewReTrmsV2(object, newdata, re.form = NULL, na.action = na.pass,
  allow.new.levels = FALSE)

predict.MEglmTree(object, newdata, type = c("prob", "class")[1], ...)

predict.MEgbm(object, newdata, type = c("prob", "raw")[1], ...)

predict.MEgbmRules(object, newdata, type = c("prob", "class")[1], ...)

predict.MErfRules(object, newdata, type = c("prob", "class")[1], ...)

predict.MEmixgbm(object, newdata, type = c("prob", "class")[1], ...)
}
\arguments{
\item{object}{Fitted model from MECtree.}

\item{newdata}{A new input data frame.}

\item{type}{of prediction: "prop" for probabilities and "class" for class labels.}

\item{...}{Further arguments passed to or from other methods.}
}
\value{
A list with items 
\item{prob}{predicted class probabilities}
\item{class}{predicted class memberships obtained by thresholding 
class probabilities at the prevalence rate of the positive class}
}
\description{
Predictions for MEml  
Make predictions using fitted MEml models: MEgbm, MEgbmrules, MErfrules, MEglmtree, MECTree, etc.
}
\author{
Che Ngufor Ngufor.Che@mayo.edu
}
