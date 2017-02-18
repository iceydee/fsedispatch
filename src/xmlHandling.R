library("XML")
library("RCurl")
library("digest")

cleanInt <- function(data, col) {
  data[,col] <- as.integer(as.character(data[,col]))
  return (data)
}

cleanDouble <- function(data, col) {
  data[,col] <- as.double(as.character(data[,col]))
  return (data)
}

cleanChar <- function(data, col) {
  data[,col] <- as.character(data[,col])
  return (data)
}

clean <- function(data, v) {
  for (n in 1:ncol(data)) {
    type <- v[n]
    if (type == "int") {
      data <- cleanInt(data, n)
    } else if (type == "double") {
      data <- cleanDouble(data, n)
    } else {
      data <- cleanChar(data, n)
    }
  }
  return (data)
}

# maxAge: -1 = always fetch, 0 = cache forever, 1+ = acceptable age in minutes
fetchXML <- function(url, name, maxAge = -1, getRawXML = FALSE) {
  path <- pathFromName(name)
  if (!validFileExists(path, maxAge)) {
    data <- getURL(url)
    if (!exists("apiQueryCount")) {
      apiQueryCount <<- 0
    }
    apiQueryCount <<- apiQueryCount + 1
    if (apiQueryCount >= 10) {
      cat("Too many api requests, sleeping...\n")
      for (i in 60:1) {
        cat(sprintf("%i seconds remaining\n", i))
        Sys.sleep(1)
      }
      apiQueryCount <<- 0
      cat("And we're back in business.\n")
    }
    if (grepl("<Error>", data)) {
      stop(data)
    }
    file.create(path, overwrite = TRUE)
    fileCOnn <- file(path)
    writeLines(data, fileCOnn)
    close(fileCOnn)
  }
  if (getRawXML) {
    return (xmlParse(path))
  }
  return (xmlToDataFrame(path))
}

safeLongName <- function(name) {
  if (nchar(name) < 50) {
    return (name)
  }
  return (sha1(name))
}

pathFromName <- function(name, extension = "xml") {
  name <- tolower(gsub("[^[:alnum:]]", "-", name))
  return (sprintf("./data/%s.%s", name, extension))
}

fileAge <- function(path) {
  info <- file.info(path)
  dt <- difftime(Sys.time(), info$mtime, units = "mins")
  return (ceiling(as.double(dt)))
}

validFileExists <- function(path, maxAge = -1) {
  if (maxAge < 0) {
    return (F)
  }
  if (!file.exists(path)) {
    return (F)
  }
  if (maxAge == 0) {
    return (T)
  }
  return (fileAge(path) < maxAge)
}
