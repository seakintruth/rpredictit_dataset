pacman::p_load(rpredictit, DBI, RSQLite, png)
scriptFileName <- function() {
    # https://stackoverflow.com/a/32016824/2292993
    cmdArgs <- commandArgs(trailingOnly = FALSE)
    needle <- "--file="
    match <- grep(needle, cmdArgs)
    tryAlternate <- FALSE
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars <- ls(sys.frames()[[1]])
        if (exists(ls_vars)) {
            if ("fileName" %in% ls_vars) {
                # Source'd via RStudio
                return(normalizePath(sys.frames()[[1]]$fileName))
            } else {
                tryAlternate <- TRUE
            }
        } else {
            tryAlternate <- TRUE
        }
    }
    if (tryAlternate){
        if (!is.null(sys.frames()[[1]]$ofile)) {
            # Source'd via R console
            return(normalizePath(sys.frames()[[1]]$ofile))
        } else {
            # RStudio Run Selection
            # http://stackoverflow.com/a/35842176/2292993
            pth = rstudioapi::getActiveDocumentContext()$path
            if (pth!='') {
                return(normalizePath(pth))
            } else {
                # RStudio Console
                tryCatch(
                    {
                        pth <- rstudioapi::getSourceEditorContext()$path
                        pth <- normalizePath(pth)
                    }, error <- function(e) {
                        # normalizePath('') issues warning/error
                        pth <- ''
                    }
                )
                return(pth)
            }
        }
    }
}
tmpFileName <- try(scriptFileName())
if (class(tmpFileName)=="try-error"){
    message("WARNING:	Unable to find script path automatically")
    #projectDirectory <- file.path("/media","jeremy","250GbUsb","data","r","predictit")
    projectDirectory <- file.path("P:","data","r","predictit")
} else {
    projectDirectory <- dirname(tmpFileName)
}

source(file.path(projectDirectory,"01_getClosedMarketData.R"))
source(file.path(projectDirectory, "02_getActiveMarketHourly.R"))
source(file.path(projectDirectory, "03_readMarketDataFromDb.R"))
source(file.path(projectDirectory, "04_analyzeClosedMarkets.R"))
