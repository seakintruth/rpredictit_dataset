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
# timestamp is the date/time the data was pulled...
closed.results.original <- closed.results
# Start wrangling!
# update our timestamp and date column to be a date/time value
closed.results$timeStamp <- closed.results$timeStamp %>%
    lubridate::as_datetime()

# convert dateEnd column to dateTime
# most common format is:   ymd_hms() , some are not,  attempting to sue guess_formats
closed.results$dateEnd <- closed.results$dateEnd %>% lubridate::ymd_hms()
closed.results$dateEnd %>% summary()

# nearly a quarter of all data has no end date, so we can't use this value

# finding character values that are shorter than expected...
closed.results$dateEndCharacterLength <- closed.results.original$dateEnd %>%
    stringr::str_length()

dateEndShort <- closed.results %>%
    dplyr::select(dateEndCharacterLength,dateEnd) %>%
    dplyr::filter(dateEndCharacterLength < 19) %>%
    dplyr::select(dateEnd) %>%
    unique()

dateEndShort

# dateEndShort column contains categorical dateEndStatus text, options include
#'No End Date','see rules','See rules.','Extended','N/A', some dates are formated as mdy
validNonStandardDays <- dateEndShort %>%
    lubridate::mdy()
validNonStandardDays

# [todo] could finish the train of though of fixing missing dates, probably not worth the time

# let's pivot our method of attempting to get a contract/market end date
# we should be able to get our last date from the max date of closed.chart.data
# for each contract
closed.chart.data %>% tibble::glimpse()

# convert date column to date object
closedChartDate <- closed.chart.data %>%
    dplyr::select(contractId,date) %>%
    dplyr::mutate(date = lubridate::as_date(lubridate::ymd_hms(date)))

glimpse(closedChartDate)

# this takes a minute, but it worked!
closedChartDate_max <- closedChartDate %>%
    group_by(contractId) %>%
    summarise(lastDate = max(date))

# set our keys to the same name so we can join on that.
closedChartDate_max <- closedChartDate_max %>%
    dplyr::rename(contract_id = contractId)

glimpse(closed.results)
glimpse(closedChartDate_max)
closed.results.endDate <-  closed.results %>%
    dplyr::full_join(closedChartDate_max,by="contract_id")

tibble::glimpse(closed.results.endDate)
colSums(is.na(closed.results.endDate))
# we still have 5k and 6k missing last date so lets dplyr::coalesce()
# to see if we gained anything from the chart data

# closed.results.endDate$lastDate
closed.results.endDate$lastEndDate <-  dplyr::coalesce(
    closed.results.endDate$lastDate,
    closed.results.endDate$dateEnd %>% as_date()
)

colSums(is.na(closed.results.endDate))
# Yup, that works, we went from 25% down to 7.7% missing end dates!
skimr::skim(closed.results.original)
skimr::skim(closed.results.endDate)

summary(closed.results.endDate$lastDate)
closed.results %>% tibble::glimpse()

# Markets with a single contract are yes/no questions
# (Yes = .99 and no= 0.01 price) let's create a cloumn identifying those

summary(closed.results)
summary(closed.results.endDate)

# let's export our cleaned up data
closed.results.clean <- closed.results.endDate %>%
    rename(
        lastTradeDate = lastDate,
        lastClosedDate = lastEndDate,
        expectedDateEnd = dateEnd,
        marketName = name,
        marketShortName = shortName,
        market_id = id
    ) %>%
    dplyr::mutate(expectedDateEnd =  lubridate::as_date(expectedDateEnd))  %>%
    select(
        -contract_image,-contract_status,-image,
        -status,-url,-querySource,-bestBuyYesCost,-bestBuyNoCost,
        -bestSellNoCost,-bestSellYesCost
    )

skimr::skim(closed.results.clean)
saveRDS(closed.results.clean,file.path(projectDirectory,"data", "closedResultsClean.RDS"))
