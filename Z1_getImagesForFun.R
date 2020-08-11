pacman::p_load(base64enc, stringr,stringi)

scriptFileName <- function() {
    # https://stackoverflow.com/a/32016824/2292993
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

if (nchar(scriptFileName())==0){
    message("WARNING:Unable to find script path automatically")
    # projectDirectory <- file.path("/media","jeremy","250GbUsb","data","r","predictit")
    projectDirectory <- file.path("D:","data","r","predictit")
} else {
    projectDirectory <- dirname(scriptFileName())
}
setwd(projectDirectory)

.getUniqueStripSmallUrl <- function(imageUrls){
    imageUrls <- unique(imageUrls)
    imageUrls <- stringr::str_replace(string = imageUrls,pattern = "small_",replacement = "")
    imageUrls <- unique(imageUrls)
    return(imageUrls)
}

.downloadImageToFile <- function(imageUrl,imageDirectory){
    #base64encode(imageConnection,0,FALSE)
    dir.create(imageDirectory, showWarnings = FALSE,recursive = TRUE)
    imageName <- stringi::stri_reverse(imageUrl)
    imageName <- stringr::str_split(imageName,pattern = "/")[[1]][1]
    imageName <- stringi::stri_reverse(imageName)
    # The next line strips the file name extention
    # imageName <- stringr::str_split(imageName,pattern = "\\.")[[1]][1]
    imageDestFile <-file.path(imageDirectory,imageName)
    if(!file.exists(imageDestFile)){
        download.file(
            url=imageUrl,
            destfile=file.path(imageDirectory,imageName),
            mode='wb',
            quiet = TRUE,
            cacheOK = TRUE
        )
    }
    return(NULL)
}

.getImages <- function(urlImageList,imageFolder){
    lapply(
        .getUniqueStripSmallUrl(
            urlImageList
        ),
        FUN=.downloadImageToFile,
        file.path(imageFolder)
    )
    return(NULL)
}

imageFolder <- file.path(projectDirectory,"images")
.getImages(closed.chart.data$contract_image,file.path(imageFolder,"contract"))
.getImages(closed.chart.data$image,file.path(imageFolder,"market"))
.getImages(closed.results$contract_image,file.path(imageFolder,"contract"))
.getImages(closed.results$image,file.path(imageFolder,"market"))
.getImages(all.market.data.now$contract_image,file.path(imageFolder,"contract"))
.getImages(all.market.data.now$image,file.path(imageFolder,"market"))
