fix_NAs <- function(df) {
  # replace any NA's in character columns by empty strings
  for (i in 1:ncol(df)) {
    coltype <- class(df[[i]])
    if (coltype=="character") {
      idx <- is.na(df[[i]])
      if (any(idx)) df[[i]][idx] <- ""
    }
  }
  return(df)
}
