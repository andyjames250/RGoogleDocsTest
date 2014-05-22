getGoogleDrive <- function(output, url, key) {
        
        ## Arguments:
        ## output = output file name
        ## url = Google Drive shared document URL
        ## key = Google Drive document key
        ## Note: Google Drive file must be shareable!
        ## Note: This function will try to use url first, then try key
        
        if(!missing(url)) {
                link <- sub("/edit?usp=sharing", "", sub("/file/d/", "/uc?export=download&id=", url, fixed = TRUE), fixed = TRUE)
        } else {
                link <- paste0("https://drive.google.com/uc?export=download&id=", key)
        }
        require(RCurl)
        bin <- getBinaryURL(link, followLocation = TRUE, ssl.verifypeer = FALSE)
        writeBin(bin, output, useBytes = TRUE)
        message(noquote(paste(output, "read into", getwd())))                        
}