#**************************************************************************
#                  Chapter 1 : Creating a Word Cloud
#**************************************************************************

#install.packages("tm")
#library(tm)
pacman::p_load(tm)

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
#                  1.2.: Preparing data for word cloud
#--------------------------------------------------------------------------
#Load up the corpus
#course_corpus <- VCorpus(DirSource("C:/Users/Kumaran Ponnambalam/Desktop/Exercise Files/courses"))
course_corpus <- VCorpus(
  DirSource(
    file.path(
      projectDirectory,
      "textAnnalysis"
    )
  )
)

#Convert to lower case
course_corpus2 <- tm_map(course_corpus, content_transformer(tolower))

#Remove punctuations
course_corpus3 <- tm_map(course_corpus2, removePunctuation)

#Remove stopwords
course_corpus4 <- tm_map(course_corpus3, removeWords, stopwords())

#Generate TF-IDF matrix
course_dtm <- DocumentTermMatrix(course_corpus4)

#Inspect to TF-IDF
inspect(course_dtm)

#Generate a frequency data frame
word_frequency <- sort(colSums(as.matrix(course_dtm)),
                       decreasing=TRUE)
df_frequency<- data.frame(word = names(word_frequency),
                          freq=word_frequency)

head(df_frequency)

#--------------------------------------------------------------------------
#                  1.3.: Displaying the Word Cloud
#--------------------------------------------------------------------------

#install.packages("wordcloud")
library(wordcloud)

#Simple wordcloud
wordcloud(df_frequency$word,
          df_frequency$freq)

#Top 10 words
wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=10, min.freq = 1)

#--------------------------------------------------------------------------
#                  1.4.: Enhancing the Word Cloud
#--------------------------------------------------------------------------

#Choose a specific font and order
wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=10, min.freq = 1,
          random.order=FALSE,
          family = "Helvatica", font = 3)

#Using a color palatte

library(RColorBrewer)

word_pal <- brewer.pal(10,"Dark2")

wordcloud(df_frequency$word,
          df_frequency$freq,
          max.words=20, min.freq = 1,
          random.order=FALSE,
          colors=word_pal, font = 3)

#--------------------------------------------------------------------------
