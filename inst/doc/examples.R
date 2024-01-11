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
knitr::knit_hooks$set(document = function(x) {
  gsub('([\n]+\\\\end\\{knitrout\\}[\n]+)', '\n\\\\end\\{knitrout\\}\\\\noindent\n', paste(x, collapse = '\n'))
})

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
P = probfun(fit)

## ----R-07-----------------------------------------------------------
P(5)

## ----R-08-----------------------------------------------------------
fit.zi = fitZIDist(roux)
fit.zi

## ----R-09, echo=FALSE, results='asis'-------------------------------
library(xtable)
raw = c(roux$data$rn/sum(roux$data$rn), 0)
tbl = cbind(0:5, raw, fitted(fit, 6), fitted(fit.zi, 6))
colnames(tbl) = c("$k$", "$P_k^{raw}$", "$P_k^{zeta}$", "$P_k^{ZIZ}$")
tbl = xtable(tbl, align = "ccccc", digits = c(0, 0, 4, 4, 4))
caption(tbl) = "Estimated probability that $k$ groups of glass would be found in shoes of a random member of the population based on the data of \\citep{roux2001}, the raw frequencies, and those produced from the zeta and ZIZ models respectively."
label(tbl) = "tab:ex1"
print(tbl,
      type="latex",
      include.rownames = FALSE,
      sanitize.text.function = function(x){x})

## ----R-010----------------------------------------------------------
ci = confint(fit)
ci$wald
ci$prof

## ----R-11-----------------------------------------------------------
ci$wald + 1
ci$prof + 1

## ----R-12,eval=FALSE,tidy=FALSE-------------------------------------
#  cr = confint(fit.zi, level = c(0.80, 0.95))
#  plot(cr[["0.95"]], type = "l")
#  polygon(cr[["0.8"]], border = "red")
#  legend("topright", lty = 1, lwd = 2, col = c("red", "black"),
#         legend = c("80%", "95%"), bty = "n")

## ----R-12a,echo=FALSE-----------------------------------------------
pdf(file = "confregion.pdf", height = 3.937008)
cr = confint(fit.zi, level = c(0.80, 0.95))
plot(cr[["0.95"]], type = "l")
polygon(cr[["0.8"]], border = "red")
legend("topright", lty = 1, lwd = 2, col = c("red", "black"),
       legend = c("80%", "95%"), bty = "n")
graphics.off()

## ----R13, eval=FALSE------------------------------------------------
#  bcr = bootCI(roux,
#         model = "ziz",
#         plot = TRUE,
#         silent = TRUE)

## ----R13a, echo=FALSE, eval=FALSE-----------------------------------
#  pdf("bootconfregion.pdf", height = 3.937008)
#  bcr = bootCI(roux,
#         model = "ziz",
#         plot = TRUE,
#         silent = TRUE)
#  graphics.off()

## ----R-15, echo=FALSE, results='asis'-------------------------------
lau = Psurveys$lau
jackson = Psurveys$jackson
tbl = cbind(0:1, lau$data$rn, jackson$data$rn)
library(xtable)
tbl = xtable(tbl, digits = 0)
align(tbl) = "cr|r|r"
label(tbl) = "tab:lau_and_jackson"
caption(tbl) = "Survey results from \\citet{lau1997} and \\citet{jackson2013}."

print(tbl,
      include.rownames = FALSE,
      include.colnames = FALSE,
      contents.only = TRUE,
      add.to.row = list(pos = list(-1, 0),
                        command = c("\\multicolumn{1}{c}{} & \\multicolumn{2}{c}{$r_n$} \\\\",
                                    "$n$ & Lau et al. & Jackson et al.  \\\\")),
      hline.after = 0,
      sanitize.colnames.function = function(x){x})

## ----R-16-----------------------------------------------------------
lau = Psurveys$lau
jackson = Psurveys$jackson
fit.lau = fitDist(lau)
fit.jackson = fitDist(jackson)
confint(fit.lau)$wald
confint(fit.jackson)$wald

## ----R-17-----------------------------------------------------------
compareSurveys(lau, jackson)

