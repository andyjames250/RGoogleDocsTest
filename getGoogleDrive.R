downloadGoogleDrive <- function(output, url, key) {
        
        ## This function downloads a text file from Google Drive into a local text file
        ## Note: Google Drive file must be shared to anyone with the link
        ## Arguments:
        ## output = output file name
        ## url = Google Drive shared document URL
        ## key = Google Drive document key
        ## Note: This function will try to use url first, then try key
        ## Note: This function will fail for any file that is too large for Google's virus scan        

        require(RCurl)
        if(!missing(url)) {
                link <- sub("/edit?usp=sharing", "", sub("/file/d/", "/uc?export=download&id=", url, fixed = TRUE), fixed = TRUE)
        } else {
                link <- paste0("https://drive.google.com/uc?export=download&id=", key)
        }
        bin <- getBinaryURL(link, followLocation = TRUE, ssl.verifypeer = FALSE)
        writeBin(bin, output, useBytes = TRUE)
        message(noquote(paste(output, "read into", getwd())))                        
}

importGoogleDrive <- function(url, key, sep = "\t", nrows = 1000) {
        
        ## This function imports a delimited text file from Google Drive into a data frame
        ## Note: Google Drive file must be shared to anyone with the link
        ## Arguments:
        ## url = Google Drive shared document URL
        ## key = Google Drive document key
        ## sep = delimiter for read.csv function
        ## Note: This function will try to use url first, then try key
        ## Note: This function will fail for any file that is too large for Google's virus scan

        require(RCurl)
        if(!missing(url)) {
                link <- sub("/edit?usp=sharing", "", sub("/file/d/", "/uc?export=download&id=", url, fixed = TRUE), fixed = TRUE)
        } else {
                link <- paste0("https://drive.google.com/uc?export=download&id=", key)
        }
        bin <- getBinaryURL(link, followLocation = TRUE, ssl.verifypeer = FALSE)
        dataFrame <- read.csv(textConnection(readBin(bin, character())), sep = sep, nrows = nrows)
}

importGoogleDrive2 <- function(url, key, filePath, sep = "\t", nrows = 1000) {
        
        ## This function imports a delimited text file from Google Drive into a data frame
        ## Note: Google Drive folder must be public
        ## Note: This function will try to use url first, then try key
        ## Arguments:
        ## url = Google Drive shared folder URL
        ## key = Google Drive folder key
        ## filePath = path and filename relative to Google Drive shared folder
        ## sep = delimiter for read.csv function
        
        require(RCurl)
        if(!missing(url)) {
                link <- sub("&usp=sharing", filePath, sub("drive.google.com/folderview?id=", "googledrive.com/host/", url, fixed = TRUE), fixed = TRUE)
        } else {
                link <- paste0("https://googledrive.com/host/", key, filePath)
        }
        bin <- getBinaryURL(link, followLocation = TRUE, ssl.verifypeer = FALSE)
        dataFrame <- read.csv(textConnection(readBin(bin, character())), sep = sep)
}