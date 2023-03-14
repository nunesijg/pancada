
##############################################################################
# LOCALDBCACHE REFERENCE CLASS
# ----------------------------------------------------------------------------
# 
# Represents a local metadata cache for all queries from PanCaDa
# 
##############################################################################
# Copyright (C) Nunes IJG et al


LocalDbCache <- setRefClass("LocalDbCache",
                            fields = list(
                              dstables = "list", # list of data.frame objects
                              repos = "character",
                              apifmt = "character",
                              tmpfiles = "character"
                              ))

LocalDbCache$methods(
  fetch_repos = function(reposname=NA_character_, ..., verbose=TRUE)
  {
    if (is.null(reposname) || is.na(reposname))
      reposname = names(repos)[1]
    reposname = match.arg(reposname, choices = names(repos))
    reposurl = repos[reposname]
    if (verbose)
      message(sprintf("Fetching %s repository...", reposname))
    jsondt = jsonlite::fromJSON(reposurl)
    jsondt$gse = trimws(jsondt$gse, 'left', 'gse')
    dstables[[reposname]] <<- jsondt
    jsondt
  },
  get_repos = function(reposname=NA_character_, ..., verbose=TRUE)
  {
    if (is.null(reposname) || is.na(reposname))
      reposname = names(repos)[1]
    reposname = match.arg(reposname, choices = setdiff(
      intersect(names(repos), names(dstables)), "parent"
    ))
    if (reposname %in% names(dstables))
      return(dstables[[reposname]])
    fetch_repos(reposname, ..., verbose=verbose)
  },
  down_or_load = function(url, force.download=FALSE, ..., verbose=TRUE)
  {
    if (!force.download && url %in% names(tmpfiles) &&
        file.exists(tmpfiles[url]))
        return(tmpfiles[url])
    tmpfnm = tempfile("bioexprdb_cache_", fileext = paste0(".", file_ext(url)))
    dlres = download.file(url, tmpfnm, method = 'auto')
    if (!identical(dlres, 0L))
      stop(sprintf("Error %s while downloading '%s'", dlres, basename(url)))
    tmpfiles[url] <<- tmpfnm
    tmpfnm
  }
)


# Local database cache. GSEs are updated only if requested
dbcache <- LocalDbCache(dstables = list(),
                        repos = c(parent="https://sbcb.inf.ufrgs.br",
                                  cumida=paste0("https://gist.githubusercontent.com/mateuz",
                                                "/ce30e844f1a986c027c8e45c989cca40/raw",
                                                "/4f3c494104f3be11f113e22afede19b7bf120087",
                                                "/cumida.json"),
                                  barracurda=paste0("https://gist.githubusercontent.com/mateuz",
                                                    "/f87fb07050926705368c6934ddc77524/raw",
                                                    "/47063780a3cdaf671d3e678d90fcc41e73754c3c",
                                                    "/barracurda.json")),
                        apifmt = "https://sbcb.inf.ufrgs.br/compute/bioexprdb/%1$s/%2$s",
                        tmpfiles = character(0)
                        )
