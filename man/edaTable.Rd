% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edaTable1.R
\name{edaTable}
\alias{edaTable}
\title{Function to generate pdf table to check balance before and after matching in an observational study}
\usage{
edaTable(baselinevars, expvars, matched, unmatched)
}
\arguments{
\item{baselinevars}{A vector of baseline variables}

\item{expvars}{A variables defining exposure group}

\item{matched}{a matched dataset}

\item{unmatched}{an unmatched dataset (original dataset)}
}
\description{
Function to generate pdf table to check balance before and after matching in an observational study
}
\examples{
 data("Telemedicine.matchedsample");data("Telemedicine") # loading matched and unmatched datasets respectively

vars <- c("Age", "Gender", "Race1", "ESRD", "BMI", "TransplantType", "Retransplant", "Hypertension", "Dialysis", "DGF", "Induction", "PSrisk",  "Surgeon","ps","linps") # vector of baseline variables
# calling funtion
edaTable(baselinevars = vars,expvars = "Telehealth",matched = Telemedicine.matchedsample,unmatched = Telemedicine)
}

