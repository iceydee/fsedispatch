library("rvest")
library("RJSONIO")
source("./src/xmlHandling.R")

htmlPath <- function(name, p = "fbos") {
  name <- sprintf("%s-%s", p, name)
  return (pathFromName(name, extension = "html"))
}

fse.getGroups <- function(maxAge = 60 * 24 * 7) {
  path <- htmlPath('groups', Sys.getenv("FSE_USER"))
  if (!validFileExists(path, maxAge)) {
    cmd <- sprintf("PREFIX='%s' ./scrape.sh get_groups", Sys.getenv("FSE_USER"))
    system(cmd)
  }
  
  groups <- data.frame(
    name = character(),
    groupId = integer(),
    stringsAsFactors = F
  )
  groups[1,] <- list(name = "Myself", groupId = 0)
  
  table <- read_html(path) %>% html_nodes(xpath = '//*[@id="groupForm"]/table//tr')
  for (n in 2:length(table)) {
    row <- html_nodes(table[n], xpath = './/td')
    name <- html_text(row[1])
    groupLink <- html_nodes(row[3], xpath = ".//a")[1] %>% html_attr("href")
    groupId <- as.integer(strsplit(groupLink, "=")[[1]][2])
    
    groups[nrow(groups) + 1,] <- list(name = name, groupId = groupId)
  }
  
  return (groups)
}

fse.bookAssignments <- function(icao, assignment_ids, group_id = 0) {
  cmd <- sprintf("FSE_ICAO='%s' ASSIGNMENT_IDS='%s' GROUP_ID='%i' ./scrape.sh get_assignments", icao, assignment_ids, group_id)
  system(cmd)
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
  
  toFetch <- sort(unique(toFetch))
  
  if (length(toFetch) > 0) {
    fetchFiles <- list()
    maxFetch <- 150
    n <- 1
    len <- length(toFetch)
    while (n <= len) {
      last <- (n + maxFetch - 1)
      if (last > len) {
        last <- len
      }
      curFetch <- toFetch[n:last]
      
      if (length(curFetch) > 100) {
        cat(sprintf("Scraping FSE: %s...%s\n\n%i airports in total.\n", paste(head(curFetch), collapse = "-"), paste(tail(curFetch), collapse = "-"), length(curFetch)))
      } else {
        cat(sprintf("Scraping FSE for: %s\n\n%i airports in total.\n", paste(curFetch, collapse = "-"), length(curFetch)))
      }
      name <- safeLongName(paste(curFetch, collapse="-"))
      path <- sprintf("./tmp/scrape-%s.json", name)
      write(toJSON(curFetch), path)
      fetchFiles[[length(fetchFiles) + 1]] <- path
      
      n <- (n + maxFetch)
    }
    
    name <- safeLongName(paste(fetchFiles, collapse="-"))
    path <- sprintf("./tmp/scrape-list-%s", name)
    write(paste(fetchFiles, collapse="\n"), path)
    
    cmd <- sprintf("PREFIX='fbos' FSE_SCRAPE_LIST='%s' ./scrape.sh airports", path)
    system(cmd)
    unlink(path)
  }
}

fse.getFBOs <- function(icao) {
  fse.fetchAirports(icao, maxAge = 60 * 24 * 30)
  a <- read_html(htmlPath(icao, "fbos")) %>% html_nodes(xpath = '//*[@id="fboInfo"]')
  a <- html_table(a, header = TRUE, fill = TRUE)
  a <- a[[1]]
  a <- a[a$FBO > 0,]
  colnames(a) <- c("FBO", "100LL Price", "100LL Gallons", "Jet-A Price", "Jet-A Gallons", "Repair/Avionics")
  a$`100LL Price` <- sapply(1:nrow(a), function(n) {suppressWarnings(as.double(gsub("\\$", "", a$`100LL Price`[n])))})
  a$`Jet-A Price` <- sapply(1:nrow(a), function(n) {suppressWarnings(as.double(gsub("\\$", "", a$`Jet-A Price`[n])))})
  a$`100LL Gallons` <- sapply(1:nrow(a), function(n) {
    if (is.na(a$`100LL Gallons`[n])) {
      return (0)
    }
    if (a$`100LL Gallons`[n] == "unlimited") {
      return (Inf)
    } else {
      return (suppressWarnings(as.integer(a$`100LL Gallons`[n])))
    }
  })
  a$`Jet-A Gallons` <- sapply(1:nrow(a), function(n) {
    if (is.na(a$`Jet-A Gallons`[n])) {
      return (0)
    }
    if (a$`Jet-A Gallons`[n] == "unlimited") {
      return (Inf)
    } else {
      return (suppressWarnings(as.integer(a$`Jet-A Gallons`[n])))
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
