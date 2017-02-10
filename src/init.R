library("XML")

source("./src/fseconomy.R")
source("./src/earnings.R")

# fse.setUserKey("12341234")

# 1. Ask for valid aircraft types
## Pre-req: download up-to-date list of aircraft
aircraft <- fse.getAircraft()

# 2. Ask for regions/timezones
# 3. Ask for start time and duration
# 4. Ask for preference (long / short-hops ok)