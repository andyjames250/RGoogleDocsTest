downloadGoogleDrive <- function(output, url, key) {
        
        ## This function downloads a text file from Google Drive into a local text file
        ## Arguments:
        ## output = output file name
        ## url = Google Drive shared document URL
        ## key = Google Drive document key
        ## Note: Google Drive file must be shareable
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

importGoogleDrive <- function(url, key, sep = "\t") {
        
        ## This function imports a text file from Google Drive into a data frame
        ## Arguments:
        ## url = Google Drive shared document URL
        ## key = Google Drive document key
        ## sep = delimiter for read.csv function
        ## Note: Google Drive file must be shareable
        ## Note: This function will try to use url first, then try key
        
        if(!missing(url)) {
                link <- sub("/edit?usp=sharing", "", sub("/file/d/", "/uc?export=download&id=", url, fixed = TRUE), fixed = TRUE)
        } else {
                link <- paste0("https://drive.google.com/uc?export=download&id=", key)
        }
        require(RCurl)
        bin <- getBinaryURL(link, followLocation = TRUE, ssl.verifypeer = FALSE)
        dataFrame <- read.csv(textConnection(readBin(bin, character())), sep = sep)
}

importGoogleDrive2 <- function(url, key, sep = "\t", nrows = 1000) {
        
        ## This function imports a text file from Google Drive into a data frame
        ## Arguments:
        ## url = Google Drive shared document URL
        ## key = Google Drive document key
        ## sep = delimiter for read.csv function
        ## Note: Google Drive file must be shareable
        ## Note: This function will try to use url first, then try key
        
        require(stringr)
        require(RCurl)
        
        if(!missing(url)) {
                link <- sub("/edit?usp=sharing", "", sub("/file/d/", "/uc?export=download&id=", url, fixed = TRUE), fixed = TRUE)
        } else {
                link <- paste0("https://drive.google.com/uc?export=download&id=", key)
        }

        bin <- getBinaryURL(link, followLocation = TRUE, ssl.verifypeer = FALSE)
        result <- readBin(bin, character())
        confirmRegex <- "confirm=[[:alnum:]]{1,}&"
        m <- regexpr(confirmRegex, result)
        if(m[1] > -1) {
                confirmText <- regmatches(result, m)
#                 confirmCode1 <- str_extract(x, confirmRegex)
#                 confirmCode2 <- strsplit(confirmCode1, "=")
#                 confirmCode3 <- confirmCode[[1]][2]
#                 confirmCode4 <- strsplit(confirmCode3, "&")
#                 confirmCode5 <- confirmCode4[[1]][1]
                link <- sub("id=", paste0(confirmText, "id="), link, fixed = TRUE)
                bin2 <- getBinaryURL(link, followLocation = TRUE, ssl.verifypeer = FALSE)
                dataFrame <- read.csv(textConnection(readBin(bin2, character())), sep = sep, nrows = nrows)
        } else {
                dataFrame <- read.csv(textConnection(readBin(bin, character())), sep = sep, nrows = nrows)
        }
}