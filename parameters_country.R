# ---------------------------------------------------------
# Opinion distributions and survey-derived parameters
#
# Source:
# International climate opinion survey data (2024)
#
# Countries:
# ITA, MEX, IND, CHN
#
# Notes:
# - Percentages converted to fractions
# - Midpoints used for reported ranges
# - Opinion distributions normalized to sum to 1
# - Some mappings are approximate proxies
# ---------------------------------------------------------


# =========================================================
# Helper functions
# =========================================================

normalize <- function(x) {
  x / sum(x)
}

midpoint <- function(a, b) {
  (a + b) / 2
}


# =========================================================
# Opinion distributions
#
# Mapping:
# strengthen -> support
# keep same  -> neutral
# weaken     -> oppose
# =========================================================

ita_op <- normalize(c(
  opp  = midpoint(1, 2),
  neut = midpoint(5, 7),
  supp = midpoint(90, 92)
))

mex_op <- normalize(c(
  opp  = midpoint(1, 2),
  neut = midpoint(4, 6),
  supp = midpoint(92, 94)
))

ind_op <- normalize(c(
  opp  = midpoint(2, 3),
  neut = midpoint(10, 12),
  supp = midpoint(82, 86)
))

chn_op <- normalize(c(
  opp  = midpoint(2, 3),
  neut = midpoint(15, 20),
  supp = midpoint(75, 80)
))


frac_opp <- c(
  ITA = ita_op["opp"],
  MEX = mex_op["opp"],
  IND = ind_op["opp"],
  CHN = chn_op["opp"]
)

frac_neut <- c(
  ITA = ita_op["neut"],
  MEX = mex_op["neut"],
  IND = ind_op["neut"],
  CHN = chn_op["neut"]
)

frac_supp <- c(
  ITA = ita_op["supp"],
  MEX = mex_op["supp"],
  IND = ind_op["supp"],
  CHN = chn_op["supp"]
)


# Validation check
opinion_check <- frac_opp + frac_neut + frac_supp
print(opinion_check)


# =========================================================
# adoptersfrac
#
# Proxy:
# proportion wanting stronger climate commitments
# =========================================================

adoptersfrac <- c(
  ITA = 0.93,
  MEX = 0.88,
  IND = 0.77,
  CHN = 0.73
)


# =========================================================
# nadopters
#
# Approximate count derived from:
# adoptersfrac * respondents
# =========================================================

nadopters <- c(
  ITA = 837,
  MEX = 792,
  IND = 924,
  CHN = 876
)


# =========================================================
# pbc (perceived behavioral control)
#
# Weak qualitative proxy from:
# "Big life decisions affected by climate change"
#
# Mapping:
# low = 0.25
# moderate = 0.50
#
# TODO:
# replace with validated behavioral-control measure
# =========================================================

pbc <- c(
  ITA = 0.25,
  MEX = 0.50,
  IND = 0.50,
  CHN = 0.50
)


# =========================================================
# mitigation support
#
# Proxy from:
# "replace fossil fuels quickly"
#
# TODO:
# improve with direct mitigation-policy survey data
# =========================================================

mitigation <- c(
  ITA = 0.89,
  MEX = NA,
  IND = NA,
  CHN = 0.80
)


# =========================================================
# natural variability perception
#
# Weak proxy from:
# frequency of thinking about climate change
#
# Midpoints used for ranges.
#
# TODO:
# identify better proxy for perceived variability
# =========================================================

naturalvariability <- c(
  ITA = midpoint(50, 60) / 100,
  MEX = midpoint(55, 65) / 100,
  IND = midpoint(35, 45) / 100,
  CHN = midpoint(65, 75) / 100
)


# =========================================================
# anomaly perception
#
# Proxy from:
# "more worried than last year"
# =========================================================

anomaly <- c(
  ITA = 0.57,
  MEX = 0.77,
  IND = 0.47,
  CHN = 0.39
)