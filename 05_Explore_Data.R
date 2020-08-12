# Ensure we can use package management
if(!require(pacman)){install.packages(pacman)}
# Load all of this script's packages
pacman::p_load(ISLR, SmartEDA, statip,fs,naniar,
               ggplot2,dplyr,simputation,lubridate,
               dplyr, lubridate, stats, grid, imputeTS,
               randomForest,missForest,mice,VIM,
               Amelia,Hmisc,mi)


midRangeFilter <- 0.2

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

#load our data set if not present
if (!exists("closed.results")) {
  projectDirectory %>%
    file.path("03_readMarketDataFromDb.R") %>%
    source(echo=TRUE)
}


market.obs <- closed.results %>%
  dplyr::group_by(id,name,shortName,url,image,timeStamp) %>%
  count()

contract.obs <- closed.results %>%
  dplyr::select(
    timeStamp,
    dateEnd,
    id,
    contract_id,
    contract_image,
    contract_name,
    contract_shortName,
    lastTradePrice,
    bestBuyYesCost,
    bestBuyNoCost,
    bestSellYesCost,
    bestSellNoCost,
    lastClosePrice
  )

contractMidFilter <- contract.obs %>%
  dplyr::filter(lastTradePrice < (1-midRangeFilter)) %>%
  dplyr::filter(lastTradePrice > midRangeFilter)

contractClosedYes <- contract.obs %>%
  dplyr::filter(lastTradePrice >= (1-midRangeFilter))
contractClosedNo <- contract.obs %>%
  dplyr::filter(lastTradePrice <= (midRangeFilter))

contractClosedYes_Summary <- contractClosedYes %>%
  select(lastTradePrice,timeStamp)
contractClosedNo_Summary <- contractClosedNo %>%
  select(lastTradePrice,timeStamp)

contractClosedYes$timeStamp <- lubridate::as_datetime(contractClosedYes$timeStamp)
contractClosedNo$timeStamp <- lubridate::as_datetime(contractClosedNo$timeStamp)

# --------------------------------------------------------------------------
# Daily Grouping Frequency
contractClosedYesDaily <- contractClosedYes %>%
  select(dateEnd,lastTradePrice) %>%
  mutate(closedDay=lubridate::as_datetime(contractClosedYes$dateEnd)) %>%
  select(closedDay,lastTradePrice)
contractClosedNoDaily <- contractClosedNo %>%
  select(dateEnd,lastTradePrice) %>%
  mutate(closedDay=lubridate::as_datetime(contractClosedNo$dateEnd)) %>%
  select(closedDay,lastTradePrice)

contractClosedYesDailyCount <- contractClosedYesDaily %>%
  group_by(closedDay) %>%
  tally() %>%
  dplyr::filter(!is.na(closedDay)) %>%
  rename('Contract Count' = n,'Planned Close Date' = closedDay)
contractClosedNoDailyCount <- contractClosedNoDaily %>%
  group_by(closedDay) %>%
  tally() %>%
  dplyr::filter(!is.na(closedDay)) %>%
  rename('Contract Count' = n,'Planned Close Date' = closedDay)

# Plot Points
plot(contractClosedYesDailyCount,type="p",col="red")
lines(contractClosedNoDailyCount,type="p",col="blue")

# Plot hist and lines
plot(contractClosedYesDailyCount,type="h", col="red",lwd=2)
lines(stats::lowess(contractClosedYesDailyCount),col="red",lwd = 5)
plot(contractClosedNoDailyCount,type="h", col="light blue",lwd = 5)
lines(stats::lowess(contractClosedNoDailyCount),col="blue",lwd = 5)
# --------------------------------------------------------------------------
# Weekly Grouping Frequency
contractClosedYesWeekly <- contractClosedYes %>%
  select(dateEnd,lastTradePrice) %>%
  mutate(closedDay=lubridate::as_datetime(contractClosedYes$dateEnd)) %>%
  mutate(closedDay=lubridate::isoweek(closedDay)) %>%
  select(closedDay,lastTradePrice)

contractClosedNoWeekly <- contractClosedNo %>%
  select(dateEnd,lastTradePrice) %>%
  mutate(closedDay=lubridate::as_datetime(contractClosedNo$dateEnd)) %>%
  mutate(closedDay=lubridate::isoweek(closedDay)) %>%
  select(closedDay,lastTradePrice)

contractClosedYesWeeklyCount <- contractClosedYesWeekly %>%
  group_by(closedDay) %>%
  tally() %>%
  dplyr::filter(!is.na(closedDay)) %>%
  rename('Contract Count' = n,'Planned Close Week' = closedDay)
contractClosedNoWeeklyCount <- contractClosedNoWeekly %>%
  group_by(closedDay) %>%
  tally() %>%
  dplyr::filter(!is.na(closedDay)) %>%
  rename('Contract Count' = n,'Planned Close Week' = closedDay)

# Plot Points
plot(contractClosedNoWeeklyCount,type="p",col="blue")
lines(contractClosedYesWeeklyCount,type="p",col="red")

# Plot hist and lines

plot(contractClosedNoWeeklyCount,type="h", col="light blue",lwd=5)
lines(stats::lowess(contractClosedNoWeeklyCount),type="l",col="blue",lwd = 5)

plot(contractClosedYesWeeklyCount,type="h", col="pink",lwd=5)
lines(stats::lowess(contractClosedYesWeeklyCount),type="l",col="red",lwd = 5)
# --------------------------------------------------------------------------
# Monthly Grouping Frequency
contractClosedYesMonthly <- contractClosedYes %>%
  select(dateEnd,lastTradePrice) %>%
  mutate(closedDay=lubridate::as_datetime(contractClosedYes$dateEnd)) %>%
  mutate(closedDay=lubridate::month(closedDay)) %>%
  select(closedDay,lastTradePrice)
contractClosedNoMonthly <- contractClosedNo %>%
  select(dateEnd,lastTradePrice) %>%
  mutate(closedDay=lubridate::as_datetime(contractClosedNo$dateEnd)) %>%
  mutate(closedDay=lubridate::month(closedDay)) %>%
  select(closedDay,lastTradePrice)

contractClosedYesMonthlyCount <- contractClosedYesMonthly %>%
  group_by(closedDay) %>%
  tally() %>%
  dplyr::filter(!is.na(closedDay)) %>%
  rename('Contract Count' = n,'Planned Close Month' = closedDay)
contractClosedNoMonthlyCount <- contractClosedNoMonthly %>%
  group_by(closedDay) %>%
  tally() %>%
  dplyr::filter(!is.na(closedDay)) %>%
  rename('Contract Count' = n,'Planned Close Month' = closedDay)

# Plot Points
plot(contractClosedNoMonthlyCount,type="p",col="blue")
lines(contractClosedYesMonthlyCount,type="p",col="red")


# Plot hist and lines
#plotYlimRange <-c(
#    contractClosedYesMonthlyCount$`Contract Count` #,
##    contractClosedNoMonthlyCount$`Contract Count`
#)
#plotYlimRange <-c(
#    min(plotYlimRange,na.rm=TRUE),
#    max(plotYlimRange,na.rm=TRUE)
#)
plot(contractClosedNoMonthlyCount,type="h", col="light blue",lwd=20)
lines(stats::lowess(contractClosedNoMonthlyCount),type="l",col="blue",lwd = 5)

plot(contractClosedYesMonthlyCount,type="h", col="pink",lwd=20)
lines(stats::lowess(contractClosedYesMonthlyCount),type="l",col="red",lwd = 5)

# --------------------------------------------------------------------------
# Export all plots
plots.png.paths <- list.files(
  list.files(
    tempdir(),
    pattern="rs-graphics",
    full.names = TRUE
  ),
  full.names = TRUE
)
setdiff(plots.png.paths,plots.png.prior.to.paths)
dir.create(file.path(projectDirectory,"plots"),recursive=TRUE,showWarnings = FALSE)
file.copy(
  setdiff(
    plots.png.paths,
    plots.png.prior.to.paths
  ),
  file.path(projectDirectory,"plots")
)
undecidedContracts <-    market.obs %>%
  dplyr::filter(id %in% contractMidFilter$id)

write.csv(
  undecidedContracts$url,
  file.path(projectDirectory,"undecided_contracts.csv"))

list.files(projectDirectory)


str(closed.results)
glimpse(closed.results)
summary(closed.results)
skimr::skim(closed.results)

# proportion of   missing values
prop_miss_case(closed.results)

# percent of missing values
pct_miss_case(closed.results)

#a numeric value that describes the number of missings in
#a given case (aka row), the percent of missings in that row
#Note: cases are sorted by percent missing
miss_case_summary(closed.results)

visdat::vis_dat(closed.results)
visdat::vis_miss(closed.results)

gg_miss_var(closed.results) + labs(y = "Number of missing values")


gg_miss_var(closed.results, facet = id )
gg_miss_upset(closed.results)

# Shadow plots!
#Note: NA vs !NA
#Note: variable names extended with suffix _NA

# Correct missing values in the lastClosedPrice column
closed_results_shadow <- bind_shadow(closed.results)
glimpse(closed.results)
glimpse(closed_results_shadow)

closed.results.verify.price <- closed.results %>%
  mutate(price.difference = (lastTradePrice - lastClosePrice)) %>%
  filter(!(price.difference == 0))

# this shows that for the 30'ish occurances  when there is a difference
# between lastTradePrice and lastClosePrice the lastTradePrice is shifted
# to the max/min

closed.results.verify.price %>%
  select(lastTradePrice,lastClosePrice,price.difference)

# So we find that there are occurances were last trade price is not lastClosePrice
summary (closed.results)

# Assuming that the lastClosePrice is a more valid
closed.results.missingLastClosedPrice <- closed_results_shadow %>%
  mutate(lastClosePriceMissing = is.na(lastClosePrice)) %>%
  filter(lastClosePriceMissing)



glimpse(closed.results.missingLastClosedPrice)
closed_results_nab <- nabular(closed.results)
glimpse(closed.results)



#verify they are identical
all.equal(closed_results_shadow, closed_results_nab)

#plot temperature when Ozone is present and when missing
ggplot(closed_results_shadow, aes(x = lastTradePrice, colour = dateEnd_NA)) + geom_density()


