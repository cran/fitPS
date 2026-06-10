## ----setup, echo=FALSE----------------------------------------------
options(width = 70)
knitr::opts_chunk$set(
  message = FALSE,
  comment = "",
  highlight = TRUE,
  prompt = TRUE,
  tidy = TRUE,
  tidy.opts = list(arrow = FALSE),
  warning = FALSE
)
suppressWarnings(suppressPackageStartupMessages(library(fitPS)))

## ----table-roux, results='asis', echo=FALSE-------------------------
library(xtable)

tbl = Psurveys$roux$data
colnames(tbl) = c("$n$", "$r_n$")
tbl = xtable(tbl, align = "rrr", digits = 0)
print(tbl, type = "latex", include.rownames = FALSE, sanitize.text.function = function(x) { x })

## ----load-data------------------------------------------------------
data("Psurveys")
roux = Psurveys$roux

## ----print-data-----------------------------------------------------
roux

## ----fit-data-------------------------------------------------------
fit = fitDist(roux)

## ----print-fit------------------------------------------------------
fit

## ----confidence-intervals-------------------------------------------
ci = confint(fit)
ci$wald
ci$prof

