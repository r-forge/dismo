\name{ModelEvaluation}

\Rdversion{1.1}

\docType{class}

\alias{ModelEvaluation}

\title{Class "ModelEvaluation"}

\description{
Class to store results of model cross-validation with presence/absence (0/1) data
}

\section{Slots}{
  \describe{
    \item{\code{presence}:}{presence data used }
    \item{\code{absence}:}{absence data used}
    \item{\code{np}:}{number of presence points}
    \item{\code{na}:}{number of absence points}
    \item{\code{auc}:}{Area under the receiver operator (ROC) curve}
    \item{\code{pauc}:}{ p-value for the AUC (for the W statistic}
    \item{\code{cor}:}{ Correlation coefficient}
    \item{\code{pcor}:}{p-value for correlation coefficient }
    \item{\code{t}:}{ vector of thresholds used to compute confusion matrices }
    \item{\code{confusion}:}{ confusion matrices }
    \item{\code{prevalence}:}{ Prevalence }
    \item{\code{ODP}:}{ Overall diagnostic power }
    \item{\code{CCR}:}{ Correct classification rate }
    \item{\code{TPR}:}{ True positive rate }
    \item{\code{TNR}:}{ True negative rate }
    \item{\code{FPR}:}{ False positive rate }
    \item{\code{FNR}:}{ False negative rate }
    \item{\code{PPP}:}{ Positive predictive power }
    \item{\code{NPP}:}{ Negative predictive power }
    \item{\code{MCR}:}{ Misclassification rate }
    \item{\code{OR}:}{ Odds-ratio }
    \item{\code{kappa}:}{ Cohen's kappa }
  }
}


\seealso{ 
\code{\link[dismo]{evaluate}} 
}

\references{
Fielding, A. H. & J.F. Bell, 1997. A review of methods for the assessment of prediction errors in conservation presence/absence models. Environmental Conservation 24: 38-49
}

\author{
Robert J. Hijmans
}

\keyword{classes}
