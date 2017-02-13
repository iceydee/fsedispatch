source("./src/xmlHandling.R")

regions <- data.frame(name = character(), minLon = integer(), maxLon = integer(), minLat = integer(), maxLat = integer(), stringsAsFactors = FALSE)
regions[1,] <- c(name = "Europe", minLon = -30, maxLon = 60, minLat = 30, maxLat = 90)
regions[2,] <- c(name = "Africa", minLon = -30, maxLon = 60, minLat = -45, maxLat = 42)
regions[3,] <- c(name = "Middle East", minLon = 20, maxLon = 75, minLat = 10, maxLat = 45)
regions[4,] <- c(name = "Northern Asia", minLon = 40, maxLon = 180, minLat = 30, maxLat = 90)
regions[5,] <- c(name = "Southern Asia", minLon = 40, maxLon = 180, minLat = 15, maxLat = 30)
regions[6,] <- c(name = "Oceania", minLon = 90, maxLon = 180, minLat = -60, maxLat = 15)
regions[7,] <- c(name = "North America", minLon = -180, maxLon = 15, minLat = 23, maxLat = 90)
regions[7,] <- c(name = "Central America / Caribbean", minLon = -120, maxLon = 30, minLat = 0, maxLat = 33)
regions[8,] <- c(name = "South America", minLon = -90, maxLon = 30, minLat = -60, maxLat = 15)
regions <- clean(regions, c(c("char"), rep("double", 4)))

limitByRegion <- function(df, region) {
  return (df[
    df$Longitude > region$minLon & df$Longitude < region$maxLon &
    df$Latitude > region$minLat & df$Latitude < region$maxLat,
  ])
}