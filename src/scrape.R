library("rvest")
library("RJSONIO")
library("plyr")
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

fse.rentAircraft <- function(icao, aircraft_reg, rent_dry = T) {
  if (rent_dry) {
    cmd <- sprintf("FSE_ICAO='%s' AIRCRAFT_REGISTRATION='%s' RENT_DRY='%i' ./scrape.sh rent_aircraft", icao, aircraft_reg, 1)
  } else {
    cmd <- sprintf("FSE_ICAO='%s' AIRCRAFT_REGISTRATION='%s' RENT_DRY='%i' ./scrape.sh rent_aircraft", icao, aircraft_reg, 0)
  }
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
    
    name <- safeLongName(paste(toFetch, collapse="-"))
    path <- sprintf("./tmp/scrape-list-%s", name)
    write(paste(fetchFiles, collapse="\n"), path)
    
    cmd <- sprintf("PREFIX='fbos' FSE_SCRAPE_LIST='%s' ./scrape.sh airports", path)
    system(cmd)
    unlink(path)
  }
}

fse.getAirlineAssignments <- function(icao, progress = function(a, b) {}) {
  if (length(icao) > 1) {
    fse.fetchAirports(icao, maxAge = 15)
    a <- list()
    for (n in 1:length(icao)) {
      a[[n]] <- fse.getAirlineAssignments(icao[n])
      progress(n / length(icao), sprintf("%.0f / %.0f", n, length(icao)))
    }
    a <- ldply(a, data.frame)
    a <- a[order(-a$Pay),]
    return (a)
  }
  
  fse.fetchAirports(icao, maxAge = 15)
  
  # Fetch assignments
  h <- read_html(htmlPath(icao, "fbos")) %>% html_nodes(xpath = '//*[@id="airportForm"]/table')
  a <- html_table(h, header = TRUE, fill = TRUE)
  a <- a[[1]]
  a$AssignmentIds <- html_nodes(h, xpath = './/*[contains(@class, "updateCheckbox")]/input') %>% html_attr("value")
  a <- a[a$Type == "A→" | a$Type == "A",]
  a <- a[,!(names(a) %in% c("Add", "Brg", "Expires", "Action"))]
  colnames(a) <- c("Pay", "FromIcao", "ToIcao", "Distance", "Commodity", "Type", "Registration", "AssignmentIds")
  if (nrow(a) > 0) {
    a$Pay <- sapply(1:nrow(a), function(n) {suppressWarnings(as.double(gsub("[\\$,]+", "", a$Pay[n])))})
    a$Distance <- sapply(1:nrow(a), function(n) {icao.distance(a$FromIcao[n], a$ToIcao[n])})
  }
  a <- clean(a, c(c("double"), rep("char", 2), c("double"), rep("char", 4)))
  
  if (nrow(a) < 1) {
    return (a)
  }
  
  # Fetch aircraft types
  b <- read_html(htmlPath(icao, "fbos")) %>% html_nodes(xpath = '//*[@id="aircraftForm"]/table')
  b <- html_table(b, header = T, fill = T)
  b <- b[[1]]
  a$Aircraft <- sapply(1:nrow(a), function(n) {b[b$Id == a$Registration[n],]$Type})
  
  return (a)
}

fse.getFBOs <- function(icao) {
  fse.fetchAirports(icao, maxAge = 60 * 24 * 30)
  a <- read_html(htmlPath(icao, "fbos")) %>% html_nodes(xpath = '//*[@id="fboInfo"]')
  a <- html_table(a, header = TRUE, fill = TRUE)
  a <- a[[1]]
  a <- a[nchar(a$FBO) > 0,]
  colnames(a) <- c("FBO", "100LL Price", "100LL Gallons", "Jet-A Price", "Jet-A Gallons", "Repair/Avionics")
  if (nrow(a) > 0) {
    a$`100LL Price` <- sapply(1:nrow(a), function(n) {
      if (is.na(a$`100LL Price`[n])) {
        return (NA)
      }
      suppressWarnings(as.double(gsub("\\$", "", a$`100LL Price`[n])))
    })
    a$`Jet-A Price` <- sapply(1:nrow(a), function(n) {
      if (is.na(a$`Jet-A Price`[n])) {
        return (NA)
      }
      suppressWarnings(as.double(gsub("\\$", "", a$`Jet-A Price`[n])))
    })
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
  }
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
