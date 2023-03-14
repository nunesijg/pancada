
##############################################################################
# CUMIDA FUNCTIONS
# ----------------------------------------------------------------------------
# 
# Query functions to download and process data from the CuMiDa repository.
# 
# More about CuMiDa in the following URL:
# [https://sbcb.inf.ufrgs.br/cumida]
# 
##############################################################################
# Copyright (C) Nunes IJG et al


#' @include LocalDbCache.R
NULL


#' @title Download from CuMiDa
#' 
#' @description Functions to download and process microarray data
#' from the CuMiDa repository.
#' 
#' @param gse (`character` or `integer`) : The GSE accession ID
#' @param tissue (`character`) : The target tissue name (case-insensitive)
#' @param platform (`logical`) : if `TRUE`, the platform data describing each
#' probe is also downloaded and included in the dataset
#' @param verbose (`logical`) : whether to print each step (`TRUE`) or run
#' silently (`FALSE`)
#' @param ... optional arguments (for compatibility only)
#' 
#' @return An [`Biobase::ExpressionSet`] object
#' 
#' @details `cumida.get` downloads a microarray dataset
#' from the CuMiDa repository and processes into an [`Biobase::ExpressionSet-class`].
#' 
#' @name cumida.get
#' @rdname cumida.get
#' 
#' @export
cumida.get <- function(gse, tissue, platform=TRUE, ..., verbose=TRUE)
{
  if (missing(tissue))
    tissue = NA_character_
  gse = trimws(toupper(gse), 'left', 'GSE')
  if (is.na(tissue))
  {
    jsondt = dbcache$get_repos('cumida', verbose=verbose)
    selgse = match(gse, jsondt$gse)
    if (is.na(selgse))
      selgse = grep(sprintf("^(?:GSE)?%s(?:$|_)",gse), jsondt$gse, perl=TRUE)[1]
    if (is.na(selgse))
      stop(sprintf("Could not find a dataset named GSE%s", gse))
    tissue = jsondt$type[selgse]
    
    gseinfo = list(gct=paste0(dbcache$repos['parent'],
                              jsondt[selgse, "downloads"]$gct),
                   gpl=paste0(dbcache$repos['parent'],
                              jsondt[selgse, "downloads"]$gpl))
  }
  else
  {
    # Use case for aliases
    tissue = switch(tolower(tissue),
                    breast = 'Breast',
                    pancreatic = 'Pancreatic',
                    liver = 'Liver',
                    leukemia = 'Leukemia',
                    prostate = 'Prostate',
                    ovary = 'Ovary',
                    brain = 'Brain',
                    bladder = 'Bladder',
                    lung = 'Lung',
                    kidney =,
                    renal = 'Renal',
                    stomach =,
                    gastric = "Gastric",
                    rectum =,
                    rectal =,
                    colorectal = "Colorectal",
                    `head/neck`=,
                    headneck = 'Throat',
                    tissue)
    
    jsonls = tryCatch(jsonlite::fromJSON(sprintf(dbcache$apifmt, gse, tissue)),
             error=function(e) stop(sprintf(paste("Could not find a dataset",
                                              "named 'GSE%s' for the tissue",
                                              "'%s'"),
                                       gse, tissue)))
    gseinfo = list(gct=jsonls$gct,
                   gpl=jsonls$gpl)
  }
  gctfile = dbcache$down_or_load(url = gseinfo$gct, ..., verbose=verbose)
  gplfile = NULL
  if (platform)
    gplfile = dbcache$down_or_load(url = gseinfo$gpl, ..., verbose=verbose)
  eset = readGCT(gctfile,
                 gpltable = if (platform) gplfile else NULL)
  eset
}
