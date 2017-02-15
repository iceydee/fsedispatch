library("rvest")
source("./src/xmlHandling.R")

htmlPath <- function(name, p = "fbos") {
  name <- sprintf("%s-%s", p, name)
  return (pathFromName(name, extension = "html"))
}

fse.fetchAirports <- function(icaos, maxAge = 60 * 24 * 30) {
  toFetch <- vector()
  for (n in 1:length(icaos)) {
    a <- icaos[n]
    path <- htmlPath(a, 'fbos')
    if (!validFileExists(path, maxAge)) {
      toFetch[length(toFetch) + 1] <- a
    }
  }
  
  if (length(toFetch) > 0) {
    cat(sprintf("Scraping FSE for: %s\n", paste(toFetch, collapse = "-")))
    system(sprintf("PREFIX='fbos' FSE_ICAO='%s' ./scrape.sh >/dev/null", paste(toFetch, collapse="-")))
  }
}

fse.getFBOs <- function(icao) {
  fse.fetchAirports(icao, maxAge = 60 * 24 * 30)
  a <- read_html(htmlPath(icao, "fbos")) %>% html_nodes(xpath = '//*[@id="fboInfo"]')
  a <- html_table(a, header = TRUE, fill = TRUE)
  a <- a[[1]]
  a <- a[a$FBO > 0,]
  colnames(a) <- c("FBO", "100LL Price", "100LL Gallons", "Jet-A Price", "Jet-A Gallons", "Repair/Avionics")
  a$`100LL Price` <- sapply(1:nrow(a), function(n) {as.double(gsub("\\$", "", a$`100LL Price`[n]))})
  a$`Jet-A Price` <- sapply(1:nrow(a), function(n) {as.double(gsub("\\$", "", a$`Jet-A Price`[n]))})
  a$`100LL Gallons` <- sapply(1:nrow(a), function(n) {
    if (is.na(a$`100LL Gallons`[n])) {
      return (0)
    }
    if (a$`100LL Gallons`[n] == "unlimited") {
      return (Inf)
    } else {
      return (as.integer(a$`100LL Gallons`[n]))
    }
  })
  a$`Jet-A Gallons` <- sapply(1:nrow(a), function(n) {
    if (is.na(a$`Jet-A Gallons`[n])) {
      return (0)
    }
    if (a$`Jet-A Gallons`[n] == "unlimited") {
      return (Inf)
    } else {
      return (as.integer(a$`Jet-A Gallons`[n]))
    }
  })
  return (a)
}

fse.icaoHasFBO <- function(icao) {
  a <- fse.getFBOs(icao)
  a <- a[a$FBO != "Local market",]
  return (nrow(a) > 0)
}

fse.fuelPrice <- function(icao, fuelType = "100LL", minVolume = 0) {
  a <- fse.getFBOs(icao)
  if (fuelType == "100LL") {
    a <- a[a$`100LL Gallons` > minVolume,]
    a <- a[order(a$`100LL Price`),]
    return (a$`100LL Price`[1])
  } else {
    a <- a[a$`Jet-A Gallons` > minVolume,]
    a <- a[order(a$`Jet-A Price`),]
    return (a$`Jet-A Price`[1])
  }
}
