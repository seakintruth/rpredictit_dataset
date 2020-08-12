#**************************************************************************
#                  Creating a Word Cloud
#**************************************************************************
pacman::p_load(
  tm,
  wordcloud,
  RColorBrewer,
  extrafont,
  fontquiver,
  grDevices,
  dplyr
)

#Load current fonts
if(length(extrafont::fonts()) == 0){
  extrafont::font_import(prompt = FALSE)
}

if(interactive()){loadfonts(device="pdf")} else {loadfonts(device="win")}

#{loadfonts(device="pdf")} else {loadfonts(device="win")}

#--------------------------------------------------------------------------
#                           Get project directory
#--------------------------------------------------------------------------
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

#--------------------------------------------------------------------------
#                  Preparing data for word cloud
#--------------------------------------------------------------------------
# save uinque contract, and market names for text analysis

if(!exists("closed.results")){
    source(
    file.path(
      projectDirectory,
      "03_readMarketDataFromDb.R"
    )
  )
}

createWordCloudFromData <- function(topicalData,topicalName){
  corpusSource <- file.path(projectDirectory,"corpus",topicalName)
  corpusResults <- file.path(projectDirectory,"results","wordCloud")
  dir.create(corpusSource,recursive=TRUE,showWarnings=FALSE)
  dir.create(corpusResults,recursive=TRUE,showWarnings=FALSE)
  write.csv2(
    topicalData,
    file = file.path(
      corpusSource,
      "data.txt"
    ),
    row.names = FALSE
  )
  #Load up the corpus
  course_corpus <- VCorpus(DirSource(corpusSource))

  #Convert to lower case
  course_corpus <- tm_map(course_corpus, content_transformer(tolower))

  #Remove punctuations
  course_corpus <- tm_map(course_corpus, removePunctuation)

  #Remove stopwords
  course_corpus <- tm_map(
    course_corpus,
    removeWords,
    tolower(
      c(
        stopwords(),stopwords("SMART"),
        2000:2028,100:200,
        "many","day",month.name,
        "jan","feb","mar","apr","jun","jul","aug","sep","oct","nov","dec"
      )
    )
  )

  #Generate TF-IDF matrix
  course_dtm <- DocumentTermMatrix(course_corpus)

  #Inspect to TF-IDF
  inspect(course_dtm)

  #Generate a frequency data frame
  word_frequency <- sort(colSums(as.matrix(course_dtm)),
                         decreasing=TRUE)
  df_frequency<- data.frame(word = names(word_frequency),
                            freq=word_frequency)
  #--------------------------------------------------------------------------
  #                  1.3.: Displaying the Word Cloud
  #--------------------------------------------------------------------------
  #Using a color palatte
  word_pal <- brewer.pal(8,"Dark2")
  resultsDestinationImage <- file.path(corpusResults,paste0(topicalName,".png"))
  if(file.exists(resultsDestinationImage)){
    file.remove(resultsDestinationImage)
  }
  png(filename=resultsDestinationImage)
  # select your vfont= parameter from running: demo("Hershey")
  wordcloud(
    df_frequency$word,
    df_frequency$freq,
    scale = c(6,1),
    max.words= 40,
    min.freq = 2,
    random.order=FALSE,
    colors=word_pal,
    family = "Candara", font = 1
  )
  text(
    x=.5,y=0,
    labels=topicalName,
    family="Arial Black",font=2,cex=2
  )
  dev.off()
}

createWordCloudFromData(
  cbind(closed.results$name,closed.results$shortName),
  "AllMarketNames"
)
createWordCloudFromData(
  cbind(closed.results$contrki9act_name,closed.results$contract_shortName),
  "AllContractNames"
)

createWordCloudFromData(
  unique(cbind(closed.results$name,closed.results$shortName,closed.results$contract_name,closed.results$contract_shortName)),
  "AllMarketAndContractNames"
)
