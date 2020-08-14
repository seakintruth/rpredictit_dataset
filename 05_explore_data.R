# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
# Load all of this script's packages
pacman::p_load(tidyverse,ggplot)

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

if (!exists("closed.results.clean")) {
  projectDirectory %>%
    file.path("04_wrangel_data.R") %>%
    source(echo=TRUE)
}

str(closed.results.clean)
glimpse(closed.results.clean)
summary(closed.results.clean)
skimr::skim(closed.results.clean)

#: where is the data with visdat for our missing dates
closed.results.clean %>%
  dplyr::select(
    expectedDateEnd, lastTradeDate,lastClosedDate
  ) %>%
  rename(lastDateCoalesed=lastClosedDate)  %>%
  visdat::vis_dat( )

closed.results.clean %>%
  dplyr::select(
    expectedDateEnd, lastTradeDate,lastClosedDate
  ) %>%
  rename(lastDateCoalesed=lastClosedDate)  %>%
visdat::vis_miss()

closed.market.count.contracts <- closed.results.clean %>%
  dplyr::select(market_id) %>%
  dplyr::group_by(market_id) %>%
  dplyr::count() %>%
  dplyr::rename(Count_of_Contracts=n
)

boxplot(closed.market.count.contracts$Count_of_Contracts)
plot(closed.market.count.contracts$Count_of_Contracts)

closed.market.many.contracts <- closed.market.count.contracts %>%
  dplyr::filter(Count_of_Contracts>10 & Count_of_Contracts<50 )

boxplot(closed.market.many.contracts$Count_of_Contracts)
summary(closed.market.many.contracts)
plot(closed.market.many.contracts$Count_of_Contracts)

# add our count of contracts to the cleaned dataset
closed.results.clean <- closed.results.clean %>%
  dplyr::full_join(closed.market.count.contracts, by="market_id")

#  glimpse(closed.results.clean)
 closed.results.clean

# filter for the trump tweet contracts
trump.tweet.markets <- closed.results.clean %>%
  mutate(marketName = stringr::str_to_lower(marketName)) %>%
  filter(stringr::str_detect(marketName,pattern = "trump")) %>%
  filter(stringr::str_detect(marketName,pattern = "tweet")) %>%
  filter(stringr::str_detect(marketName,pattern = "noon")) %>%
  filter(stringr::str_detect(marketName,pattern = "mention",negate=TRUE)) %>%
  filter(stringr::str_detect(marketName,pattern = "trumpjr",negate=TRUE)) %>%
  mutate(contractNameLength= stringr::str_length(contract_shortName)) %>%
  filter(contractNameLength < 15) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\ or\\ more", "+")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\ or\\ fewer", "-")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\ or\\ less", "-")) %>%
  mutate(marketShortName = stringr::str_to_lower(marketShortName)) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"\\?", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"noon\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"noon", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"\\@realdonaldtrump\\ tweets\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace(marketShortName,"trump\\ tweets\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace_all(marketShortName,"\\ ", "")) %>%
  mutate(marketShortName = stringr::str_replace_all(marketShortName,"to", "\\-")) %>%
  mutate(contract_shortName = stringr::str_replace_all(contract_shortName,"\\ ", "")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\-(.*)", "")) %>%
  mutate(contract_shortName = stringr::str_replace(contract_shortName,"\\+", ".1")) %>%
  mutate(contract_lowerBinValue = as.numeric(contract_shortName))


trump.tweet.markets.tmp <- trump.tweet.markets %>%
  filter(!is.na(lastClosedDate)) %>%
  group_by(market_id) %>%
  summarise(lastMarketClosedDate = max(lastClosedDate))

trump.tweet.markets <- full_join(trump.tweet.markets, trump.tweet.markets.tmp,by="market_id")

filterRange <- .15
trump.tweet.markets.yes <- trump.tweet.markets %>%
  filter(lastTradePrice >= (1-filterRange))
trump.tweet.markets.maybe <- trump.tweet.markets %>%
  filter(lastTradePrice < (1-filterRange) & lastTradePrice >=(0+filterRange))
trump.tweet.markets.no <- trump.tweet.markets %>%
  filter(lastTradePrice < (0+filterRange))

unique(trump.tweet.markets.yes$contract_lowerBinValue)
view(trump.tweet.markets.yes)
plot(trump.tweet.markets.yes$lastMarketClosedDate,trump.tweet.markets.yes$contract_lowerBinValue)
summary(trump.tweet.markets.yes$contract_lowerBinValue)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.
# 19.00   50.00   70.00   96.82  130.00  484.00

# number of years of data based on number of weeks of yes contracts
datasetDaysByWeek <- length(trump.tweet.markets.yes$contract_lowerBinValue)*7
datasetDaysByWeek/365
# 3.547 years


#number of years of data based on last date - first date
datasetDaysByDate <- (
  trump.tweet.markets.yes$lastMarketClosedDate %>%
    max()
  - trump.tweet.markets.yes$lastMarketClosedDate %>%
    min()
) %>%
as.numeric()

datasetDaysByDate/365
# 3.389 years

NumberOfExtraWeeks <- (datasetDaysByWeek - datasetDaysByDate)/7
# 8.286 weeks
# [todo] need to find out why this descrepancy exists, it's not obvious at first glance

# There appears to be a major divergance in values 1/3rd of the way through 2019
# So let's treat these as two different processes
trump.tweet.markets.yes.section1 <- trump.tweet.markets.yes %>%
  filter(lastMarketClosedDate < lubridate::mdy("3/1/2019"))

trump.tweet.markets.yes.section2 <- trump.tweet.markets.yes %>%
  filter(lastMarketClosedDate >= lubridate::mdy("3/1/2019"))

plot(trump.tweet.markets.yes.section1$lastMarketClosedDate,trump.tweet.markets.yes.section1$contract_lowerBinValue)
plot(trump.tweet.markets.yes.section2$lastMarketClosedDate,trump.tweet.markets.yes.section2$contract_lowerBinValue)

plot(trump.tweet.markets$lastMarketClosedDate,trump.tweet.markets$contract_lowerBinValue)
plot(trump.tweet.markets.no$lastMarketClosedDate,trump.tweet.markets.no$contract_lowerBinValue)

#[TODO] attempted to perform these filters as a single filter with anded regex, failed...
summary(trump.tweet.markets)

trump.tweet.markets
