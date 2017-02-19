source("./src/xmlHandling.R")

nxt <- function(r) {
  return (nrow(r) + 1)
}

regions <- data.frame(name = character(), minLon = integer(), maxLon = integer(), minLat = integer(), maxLat = integer(), stringsAsFactors = FALSE)
regions[nxt(regions),] <- c(name = "Central Europe", minLon = 6, maxLon = 24, minLat = 45.5, maxLat = 55) #
regions[nxt(regions),] <- c(name = "Western Europe", minLon = -10.5, maxLon = 8.25, minLat = 36, maxLat = 61) #
regions[nxt(regions),] <- c(name = "Eastern Europe", minLon = 12, maxLon = 30, minLat = 41.2, maxLat = 59.7) #
regions[nxt(regions),] <- c(name = "Northern Europe", minLon = -24.5, maxLon = 31.5, minLat = 50, maxLat = 80.8) #
regions[nxt(regions),] <- c(name = "Europe", minLon = -10.5, maxLon = 53, minLat = 17, maxLat = 82) #
regions[nxt(regions),] <- c(name = "Africa", minLon = -26, maxLon = 56, minLat = -37, maxLat = 36) #
regions[nxt(regions),] <- c(name = "Middle East", minLon = 20, maxLon = 75, minLat = 10, maxLat = 45)
regions[nxt(regions),] <- c(name = "Northern Asia", minLon = 40, maxLon = 180, minLat = 30, maxLat = 90)
regions[nxt(regions),] <- c(name = "Southern Asia", minLon = 40, maxLon = 180, minLat = 15, maxLat = 30)
regions[nxt(regions),] <- c(name = "Oceania", minLon = 90, maxLon = 180, minLat = -60, maxLat = 15)
regions[nxt(regions),] <- c(name = "North America", minLon = -180, maxLon = 15, minLat = 23, maxLat = 90)
regions[nxt(regions),] <- c(name = "USA withouut Alaska and Hawaii", minLon = -125, maxLon = -64, minLat = 24, maxLat = 48) #
regions[nxt(regions),] <- c(name = "USA Pacific", minLon = -125, maxLon = -113.5, minLat = 32.5, maxLat = 49) #
regions[nxt(regions),] <- c(name = "USA Mountain", minLon = -117.5, maxLon = -100, minLat = 31.2, maxLat = 49) #
regions[nxt(regions),] <- c(name = "USA Central", minLon = -107, maxLon = -84.8, minLat = 25.8, maxLat = 49) #
regions[nxt(regions),] <- c(name = "USA Eastern", minLon = -90.5, maxLon = -44.8, minLat = 24.4, maxLat = 48) #
regions[nxt(regions),] <- c(name = "Arctic", minLon = -180, maxLon = 180, minLat = 60, maxLat = 90)
regions[nxt(regions),] <- c(name = "Central America / Caribbean", minLon = -120, maxLon = 30, minLat = 0, maxLat = 33)
regions[nxt(regions),] <- c(name = "Caribbean", minLon = -87, maxLon = -59, minLat = 10, maxLat = 28) #
regions[nxt(regions),] <- c(name = "South America", minLon = -90, maxLon = 30, minLat = -60, maxLat = 15)
regions <- clean(regions, c(c("char"), rep("double", 4)))

limitByRegion <- function(df, region) {
  return (df[
    df$Longitude > region$minLon & df$Longitude < region$maxLon &
    df$Latitude > region$minLat & df$Latitude < region$maxLat,
  ])
}