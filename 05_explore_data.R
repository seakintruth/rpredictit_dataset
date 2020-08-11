pacman::p_load(ISLR, SmartEDA, statip,fs,naniar,
               ggplot2,dplyr,simputation,lubridate)

scriptFileName <- function() {
  # http://stackoverflow.com/a/32016824/2292993
  cmdArgs = commandArgs(trailingOnly = FALSE)
  needle = "--file="
  match = needle %>% grep(cmdArgs)
  if (length(match) > 0) {
    # Rscript via command line
    needle %>% sub( "", cmdArgs[match]) %>% normalizePath() %>% return()
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
            pth %>% normalizePath() %>% return()
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
            pth %>% return()
          }
        }
      }
    }
  }
}

if (scriptFileName() %>% is.null()){
  message("WARNING:Unable to find script path automatically")
  # projectDirectory <- file.path("/media","jeremy","250GbUsb","data","r","predictit")
  projectDirectory <- file.path("D:","data","r","predictit")
} else {
  projectDirectory <- scriptFileName() %>% dirname()
}

if (!exists("closed.results")) {
  projectDirectory %>%
    file.path("03_readMarketDataFromDb.R") %>%
    source(echo=TRUE)
}

str(closed.results)
glimpse(closed.results)
summary(closed.results)
skimr::skim(closed.results)

# proportion of missing values
prop_miss_case(airquality)

# percent of missing values
pct_miss_case(airquality)

#a numeric value that describes the number of missings in
#a given case (aka row), the percent of missings in that row
#Note: cases are sorted by percent missing
miss_case_summary(airquality)

#Slide : where is the data with visdat
visdat::vis_dat(closed.results)
visdat::vis_miss(closed.results)

gg_miss_var(closed.results) + labs(y = "Number of missing values")
gg_miss_var(closed.results, facet = id )
gg_miss_upset(airquality)


closedContracts <- closed.results %>%
  select(timeStamp,id,name,shortName,contract_id,dateEnd,)
  lubridate::as_datetime(closed.results$timeStamp)


