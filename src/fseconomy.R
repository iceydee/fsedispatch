source("./src/xmlHandling.R")

fse.setUserKey <- function(key) {
  userkey <<- key
}

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

fse.getAircraft <- function() {
  url <- fse.query("aircraft", "search=configs")
  a <- fetchXML(url, "aircraft", maxAge = (60 * 24 * 7))
  a <- clean(a, c(c("char"), rep("int", 23)))
  return (a)
}

fse.query <- function(query, args) {
  baseURL <- "http://server.fseconomy.net/data"
  return (sprintf("%s?userkey=%s&format=xml&query=%s&%s", baseURL, userkey, query, args))
}
