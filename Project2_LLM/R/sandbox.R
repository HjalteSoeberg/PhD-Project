### sandbox.R --- 
#----------------------------------------------------------------------
## Author: Hjalte Søberg Mikkelsen
## Created: Nov 21 2025 (10:13) 
## Version: 
## Last-Updated: Nov 21 2025 (10:31) 
##           By: Hjalte Søberg Mikkelsen
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: Try to incoporate longitudinal data with the time between datapoints
############### into a neural networ in a smart way
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(data.table)

##create example data
DT <- data.table(ID = c(1,1,1,2,2,3,3,3,3),
                 value = c(4.3,4.9,4.4,6.1,6.4,4.9,5.2,5.1,4.8),
                 date = c(as.Date('2020-01-01'),as.Date('2020-01-05'),as.Date('2020-02-01'),as.Date('2020-01-15'),as.Date('2020-03-01'),as.Date('2020-01-03'),as.Date('2020-01-10'),as.Date('2020-01-20'),as.Date('2020-02-10'))
                 )
##Add time gap
# Sort by person & date
setorder(DT, ID, date)
# Compute time differences (in days)
DT[, delta_t := as.numeric(date - shift(date, type = "lag")), by = ID]
DT[is.na(delta_t), delta_t := 0]
# Time since baseline
DT[, t_since_base := as.numeric(date - first(date)), by = ID]


## For torch
# Creates a data.table with the columns ID, features and length
# features is a matrix with all the covariates for that ID
# lenght is how many rows the 'features' matrix has
seqs <- DT[, list(
  features = list(as.matrix(.SD[, .(value, delta_t, t_since_base)])),
  length   = .N
), by = ID]


######################################################################
### sandbox.R ends here
