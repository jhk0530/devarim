.check_field <- function(df, field, types, typ, alias=NULL)
{
  dfname <- deparse(substitute(df))
  colname <- field # for type checking; replace by alias if appropriate
  if (! field %in% names(df)) {
    if (is.null(alias)) {
      ok <- FALSE
    } else {
      ok <- alias %in% names(df)
      if (ok) colname <- alias
    }
    if (!ok) stop(sprintf("'%s' does not have a column '%s'",dfname, field), call.=FALSE)
  }
  if (! class(df[[colname]]) %in% types) stop(sprintf("%s: field '%s' is not <%s>",dfname,field,typ), call.=FALSE)
}
