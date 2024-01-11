## ----R0-setup, echo=FALSE-------------------------------------------
options(width=70)
knitr::opts_chunk$set(message = FALSE,
                      #background = opcolour,
                      comment = "",
                      highlight = TRUE,
                      prompt = TRUE,
                      tidy = TRUE,
                      tidy.opts = list(arrow = FALSE),
                      warning = FALSE)
suppressWarnings(suppressPackageStartupMessages({library(fitPS)}))

## ----R-01, results='asis', echo=FALSE-------------------------------
library(xtable)

tbl = Psurveys$roux$data
colnames(tbl) = c("$n$", "$r_n$")
tbl = xtable(tbl, align = "rrr", digits = 0)
print(tbl, type="latex", , include.rownames = FALSE, sanitize.text = function(x){x})

## ----R-02-----------------------------------------------------------
data("Psurveys")
roux = Psurveys$roux

## ----R-03-----------------------------------------------------------
roux

## ----R-04-----------------------------------------------------------
fit = fitDist(roux)

## ----R-05-----------------------------------------------------------
fit

## ----R-06-----------------------------------------------------------
ci = confint(fit)
ci$wald
ci$prof

## ----R-07-----------------------------------------------------------
ci$wald + 1
ci$prof + 1

