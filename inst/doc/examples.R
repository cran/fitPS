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

## ----load-roux------------------------------------------------------
data("Psurveys")
roux = Psurveys$roux

## ----print-roux-----------------------------------------------------
roux

## ----fit-roux-------------------------------------------------------
fit = fitDist(roux)

## ----print-fit------------------------------------------------------
fit

## ----probability-function-------------------------------------------
P = probfun(fit)

## ----probability-example--------------------------------------------
P(5)

## ----fit-zero-inflated----------------------------------------------
fit.zi = fitZIDist(roux)
fit.zi

## ----zero-inflated-table, echo=FALSE, results='asis'----------------
library(xtable)
raw = c(roux$data$rn / sum(roux$data$rn), 0)
tbl = cbind(0:5, raw, fitted(fit, 6), fitted(fit.zi, 6))
colnames(tbl) = c("$k$", "$P_k^{raw}$", "$P_k^{zeta}$", "$P_k^{ZIZ}$")
tbl = xtable(tbl, align = "ccccc", digits = c(0, 0, 4, 4, 4))
caption(tbl) = "Estimated probability that k groups of glass would be found in shoes of a random member of the population based on the data of Roux et al. (2001), the raw frequencies, and those produced from the zeta and ZIZ models respectively."
label(tbl) = "tab:ex1"
print(tbl, type = "latex", include.rownames = FALSE, sanitize.text.function = function(x) { x })

## ----confidence-intervals-------------------------------------------
ci = confint(fit)
ci$wald
ci$prof

## ----confidence-region, eval=FALSE, tidy=FALSE----------------------
# cr = confint(fit.zi, level = c(0.80, 0.95))
# plot(cr[["0.95"]], type = "l")
# polygon(cr[["0.8"]], border = "red")
# legend("topright", lty = 1, lwd = 2, col = c("red", "black"),
#        legend = c("80%", "95%"), bty = "n")

## ----boot-confidence-region, eval=FALSE-----------------------------
# bcr = bootCI(roux,
#        model = "ziz",
#        plot = TRUE,
#        silent = TRUE)

## ----compare-data-table, echo=FALSE, results='asis'-----------------
lau = Psurveys$lau
jackson = Psurveys$jackson
tbl = cbind(0:1, lau$data$rn, jackson$data$rn)
library(xtable)
tbl = xtable(tbl, digits = 0)
align(tbl) = "cr|r|r"
caption(tbl) = "Survey results from Lau et al. (1997) and Jackson et al. (2013)."
print(tbl, include.rownames = FALSE, include.colnames = FALSE)

## ----compare-fits---------------------------------------------------
lau = Psurveys$lau
jackson = Psurveys$jackson
fit.lau = fitDist(lau)
fit.jackson = fitDist(jackson)
confint(fit.lau)$wald
confint(fit.jackson)$wald

## ----compare-surveys------------------------------------------------
compareSurveys(lau, jackson)

