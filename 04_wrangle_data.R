# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
# Load all of this script's packages
pacman::p_load(tidyverse)

scriptFileName <- function() {
    # http://stackoverflow.com/a/32016824/2292993
    cmdArgs = commandArgs(trailingOnly = FALSE)
    needle = "--file="
    match = grep(needle, cmdArgs)
    if (length(match) > 0) {
        # Rscript via command line
        return(normalizePath(sub(needle, "", cmdArgs[match])))
    } else {
        ls_vars = ls(sys.frames()[[1]])
        if ("fileName" %in% ls_vars) {
            # Source'd via RStudio
            return(normalizePath(sys.frames()[[1]]$fileName))
        } else {
            if (!is.null(sys.frames()[[1]]$ofile)) {
                # Source'd via R console
                return(normalizePath(sys.frames()[[1]]$ofile))
            } else {
                # RStudio Run Selection
                # http://stackoverflow.com/a/35842176/2292993
                if ("rstudioapi" %in% installed.packages()){
                    pth = rstudioapi::getActiveDocumentContext()$path
                    if (pth!='') {
                        return(normalizePath(pth))
                    } else {
                        # RStudio Console
                        tryCatch({
                            pth = rstudioapi::getSourceEditorContext()$path
                            pth = normalizePath(pth)
                        }, error = function(e) {
                            # normalizePath('') issues warning/error
                            pth = ''
                        }
                        )
                        return(pth)
                    }
                }
            }
        }
    }
}

if (is.null(scriptFileName())){
    message("WARNING:Unable to find script path automatically")
    # projectDirectory <- file.path("/media","jeremy","250GbUsb","data","r","predictit")
    projectDirectory <- file.path("P:","data","r","predictit")
} else {
    projectDirectory <- dirname(scriptFileName())
}

#load our data set if not present
if (!exists("closed.results")) {
    projectDirectory %>%
        file.path("03_readMarketDataFromDb.R") %>%
        source(echo=TRUE)
}

# Start wrangling!
# update our timestamp to be a date/time value
closed.results$timeStamp <- lubridate::as_datetime(closed.results$timeStamp)

# Markets with a single contract are yes/no questions
# (Yes = .99 and no= 0.01 price)

glimpse(closed.results)
summary(closed.results)
# update our
# unload large packages
pacman::p_unload(tidyverse)
