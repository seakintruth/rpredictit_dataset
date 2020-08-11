pacman::p_load(magick, dplyr, tidyverse,tools)
# Config ::
numberOfBatchImages=500
# for some reason when we go over 500 image batches this script slows to a crawl
op <- options(digits.secs = 90)
options(op)

# Requires install from https://imagemagick.org/script/download.php#windows
# Had to increase the limits of image magick in terminal on linux with:
# sudo nano /etc/ImageMagick-6/policy.xml
# Updated the following Values:
#    <policy domain="resource" name="memory" value="2GiB"/>
#    <policy domain="resource" name="map" value="4GiB"/>
#    <policy domain="resource" name="width" value="32MP"/>
#    <policy domain="resource" name="height" value="32MP"/>
#    <policy domain="resource" name="area" value="1GiB"/>
#    <policy domain="resource" name="disk" value="20GiB"/>
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

tmpFileName <- try(scriptFileName())
if (class(tmpFileName)=="try-error"){
    message("WARNING:	Unable to find script path automatically")
    #projectDirectory <- file.path("/media","jeremy","250GbUsb","data","r","predictit")
    projectDirectory <- file.path("P:","data","r","predictit")
} else {
    projectDirectory <- dirname(tmpFileName)
}
setwd(projectDirectory)
downloadedImagesList <- list.files(
    file.path(
        projectDirectory,"images"
    ),
    recursive = TRUE,
    full.names = TRUE,
    no.. = TRUE,
    pattern = "(*\\.jpg|*\\.png)"
)

getDistortionFromImageCompareAll <- function(
    intIndexCheck,
    imageList
){
    distortionList<-rep(NA,length(imageList))
    for (intIndex in 1:length(imageList)) {
        if(intIndex == intIndexCheck){
            distortionList[intIndex] <- NA
        } else {
            distortionList[intIndex] <- attributes(
                magick::image_compare(
                    imageList[intIndexCheck],
                    imageList[intIndex],
                    metric = "",
                    fuzz = 0
                )
            )$distortion
        }
    }
    return(distortionList)
}

if(exists("contractMarketImages")){
    rm(contractMarketImages)
}
if(exists("loadPointerToRamErrorCheck")){
    rm(loadPointerToRamErrorCheck)
}
timeSart <- Sys.time()
loadPointerToRamErrorCheck <- try(
    contractMarketImages <- magick::image_read(
        path=downloadedImagesList,
        strip = FALSE,
        depth=16
    )
)
message(
    "Image load took: ",
    difftime(Sys.time(),timeSart,units="mins"),
    " minutes for ",
    length(contractMarketImages),
    " images"
)
timeSartCompare <- Sys.time()
compareImagesInBatch <- function(
    intIndexCheck,
    imageList
){
    # for testing just looking at the first group of files!
    downloadedImagesList <- downloadedImagesListFull[1:numberOfBatchImages]
    contractMarketImageInfo$image1Compare <- getDistortionFromImageCompareAll(
        1,
        contractMarketImages
    )
}
compareImagesInBatch(
    1,
    contractMarketImages
)
message(
    "Image Compare took: ",
    difftime(Sys.time(),timeSart,units="mins"),
    " minutes for ",
    length(contractMarketImages),
    " images"
)

# # you can't save a pointer as an RDS file...
# # Don't Run: saveRDS(contractMarketImages,file.path(projectDirectory,"contractMarketImages.RDS"))
# contractMarketImageInfo <- magick::image_info(contractMarketImages)
# contractMarketImageInfo$fileName <- downloadedImagesList
# contractMarketImageInfo$md5sum <- tools::md5sum(downloadedImagesList)
#
# #contractMarketImageAttributes <- magick::image_attributes(contractMarketImages[222])
# #str(contractMarketImageAttributes)
#
# # I'm going to only use getDistorionFromImageCompareAll vs one image to check
# # that we get the same results from tools::md5sum() and tools::checkMD5sums()
# # tuns out md5checksum is too strict
# plot(contractMarketImageInfo$image1Compare)
# hist(contractMarketImageInfo$image1Compare,breaks=seq(from=0,to=1.1,by=0.005),xlim=range(0,1.1))
#
#
# imageSimilar <- contractMarketImageInfo %>%
#    filter(image1Compare > .9)
#
# #plot(imageSimilar$image1Compare)
#
# imageSimilar$fileName
#
# # View an image with either:
# magick::image_browse(contractMarketImages[1])
# magick::image_display(contractMarketImages[1])
#
