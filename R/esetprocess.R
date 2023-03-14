
##############################################################################
# EXPRESSIONSET-RELATED PARSE FUNCTIONS
# ----------------------------------------------------------------------------
# 
# Functions for I/O data processing into ExpressionSet objects.
# 
##############################################################################
# Copyright (C) Nunes IJG et al



# Reads a GPL file, returns a data.frame
readGPL <- function(gplfile)
{
  dtgpl = read.table(gplfile, sep = '\t', header = TRUE,
                     row.names = 1, skip = 0L, comment.char = "",
                     quote=NULL, check.names = FALSE, blank.lines.skip = TRUE)
  if (!is.character(dtgpl[,1,drop=TRUE]))
    dtgpl[,1] = as.character(dtgpl[,1])
  dtgpl
}


# Reads a GCT file, returns a ExpressionSet
# If a GPL is provided as a file or table, merges it into the ExpressionSet
readGCT <- function(gctfile, gpltable=NULL)
{
  
  
  dtgct = read.table(gctfile, sep = '\t', header = TRUE,
                     row.names = 1, skip = 2L,comment.char = "",
                     quote="", check.names = FALSE)
  dtgct = dtgct[,-1L,drop=FALSE]
  eset = new("ExpressionSet", exprs=as.matrix(dtgct))
  if (!identical(length(gpltable), 0L))
  {
    if (inherits(gpltable, c("character", "connection")))
      gpltable = readGPL(gpltable)
    gpltable = as.data.frame(gpltable)
    vmsels = match.bycols(rownames(dtgct), gpltable,
                          cols=c("ID", "SPOT_ID", "NAME", "GB_ACC",
                                 "TIGR_ID", "ENSEMBL_ID", "ProbeName",
                                 "ACCESSION_STRING"))
    if (!all(is.na(vmsels)))
    {
      gpltable = gpltable[vmsels,,drop=FALSE]
      rownames(gpltable) = rownames(dtgct)
      vmdt = data.frame(colnames(gpltable),
                        row.names = colnames(gpltable))
      featureData(eset) = new("AnnotatedDataFrame",
                              data=gpltable,
                              varMetadata=vmdt)
    }
  }
  eset
}
