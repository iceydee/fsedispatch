cat("FSEDispatch v0.1.0\n")
cat("------------------\n\n")

source("./src/fseconomy.R")
source("./src/earnings.R")
source("./src/regions.R")

aircraft <- fse.getAircraft("Cessna 404 Titan")
region <- regions[5,]
rentalAircraft <- fse.findRentalAircraft(aircraft$MakeModel, waterOk = F)
rentalAircraft <- limitByRegion(rentalAircraft, region)
leg1 <- getRankedAssignments(rentalAircraft, 0, maxDistance)
leg2 <- getRankedAssignments(rentalAircraft, minDistance, maxDistance, leg1$ToIcao, leg1$FromIcao)
results <- gatherResults(leg1, leg2, maxDistance)
saveRDS(results, file = "./data/canned_data.rds")
