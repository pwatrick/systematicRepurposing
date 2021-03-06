% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measure_treatment_effect.R
\name{measure_treatment_effect}
\alias{measure_treatment_effect}
\title{Measure treatment effect in clinical data
Last updated: 2021-02-12}
\usage{
measure_treatment_effect(
  .data,
  biomarker_tbl,
  drug_name,
  phenotype,
  biomarker_name,
  cohort_vector
)
}
\arguments{
\item{.data}{A tibble with columns, c("person_id", "drug_exposed", "treatment_new_exposed")}

\item{biomarker_tbl}{A tibble with columns, c("person_id", "biomarker_baseline_value", "biomarker_treatment_value", "biomarker_change", "biomarker_name")}

\item{drug_name}{A string, e.g., "Lisinopril"}

\item{phenotype}{A string, e.g., "Hyperlipidemia"}

\item{biomarker_name}{A string, e.g., "LDL cholesterol"}

\item{cohort_vector}{A vector, e.g,. c("Primary Analysis", "Subanalysis 1", "Subanalysis 2", "Subanalysis 3")}
}
\description{
Measure treatment effect in clinical data
Last updated: 2021-02-12
}
