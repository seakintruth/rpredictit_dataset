# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
pacman::p_load(RSQLite, DBI,magrittr)

pacman::p_load(magrittr)
# Function scriptFileName() attempts to find the current scripts path to use relative path
# Checks to see if script is source'd via:
# command line, R console, RStudio,RStudio Console,RStudio Run Selection
# returns '' if we are unable to find
# http://stackoverflow.com/a/35842176/2292993
# build and view db with https://sqlitebrowser.org/
scriptFileName <- function() {
    tryCatch({
        cmdArgs <- commandArgs(trailingOnly = FALSE)
        matchFound = "--file=" %>% grep(cmdArgs)
        if (matchFound %>% length() > 0) {
            "--file=" %>% sub("", cmdArgs[matchFound]) %>% normalizePath() %% return()
        } else if ("fileName" %in% (sys.frames()[[1]] %>% ls())){
            sys.frames()[[1]]$fileName %>% normalizePath() %>% return()
        } else if (sys.frames()[[1]]$ofile %>% is.null()){
            if (pacman::p_exists(rstudioapi)){
                pth = rstudioapi::getActiveDocumentContext()$path
                if (pth == '') {
                    rstudioapi::getSourceEditorContext()$path %>%
                        normalizePath() %>% return()
                } else {pth %>% normalizePath() %>% return()}
            } else {
                message("WARNING:Unable to find script path automatically select this script.")
                file.choose() %>% return()
            }
        } else {
            sys.frames()[[1]]$ofile %>% normalizePath() %>% return()
        }
    }, error = function(e) {
        message("WARNING:Unable to find script path automatically select this script.")
        file.choose() %>% return()
    })
}
projectDirectory <- dirname(scriptFileName())

.open.predictit.db <- function(){
    db <- DBI::dbConnect(
        RSQLite::SQLite(),
        dbname=file.path(projectDirectory,"data","openMarkets.sqlite")
    )

    return(db)
}

readOpenMarketDataFromDb <- function(){
    db <- .open.predictit.db()
    result <- RSQLite::dbSendQuery(
        conn=db,
        "SELECT * FROM marketObservation"
    )
#    dbClearResult(result)
    RSQLite::dbDisconnect(db)
    return(result)
}

open.results <- readOpenMarketDataFromDb()
saveRDS(open.results,file.path(projectDirectory,"data","openResults.RDS"))
