
##############################################################################
# MATRIX HELPER FUNCTIONS
# ----------------------------------------------------------------------------
# 
# Functions to process and manipulate matrix and data.frames objects.
# 
##############################################################################
# Copyright (C) Nunes IJG et al


# Matches a character vector to elements from one or more columns
# Returns the matched indexes for the first matching column. Unmatched
# elements are returned as NA
match.bycols <- function(x, dtable, cols=colnames(dtable),
                         include.row.names=TRUE)
{
  vminds = if (include.row.names && !(is.null(rownames(dtable))))
    match(x, rownames(dtable)) else rep(NA_integer_, length(x))
  if (!anyNA(vminds))
    return(vminds)
  cols = intersect(cols, colnames(dtable))
  for (colnm in cols)
  {
    sel.nas = is.na(vminds)
    vminds[sel.nas] = match(x[sel.nas], getElement(dtable, colnm))
    if (!anyNA(vminds))
      break
  }
  vminds
}
