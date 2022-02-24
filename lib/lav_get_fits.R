lav_get_fits <- function(fit) {
  fits <- lavInspect(fit, what = "fit")[c("chisq.scaled", "df.scaled", "cfi.robust", "tli.robust", "rmsea.robust", "rmsea.ci.lower.robust", "rmsea.ci.upper.robust", "srmr")]
  fits <- as.data.frame(t(fits))
  fits <- round(fits, 3)
  names(fits) <- c("ChiSQ", "Degrees of Freedom", "CFI", "TLI", "RMSEA", "RMSEA Lower", "RMSEA Upper", "SRMR")
  return(fits)
}
