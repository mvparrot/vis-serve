#================================================
#--- Generating Coordinates
#--- Alwin Wang
#================================================

#--- Run the script to load the data
source("Helper Load Data.R")

test_data <- atp_serves[2,]

#--- Run the script to get coordinate functions
source("Helper Generate Coordinates.R")



x1 <- generate()

for(row in 1:nrow(data)) {
    
}