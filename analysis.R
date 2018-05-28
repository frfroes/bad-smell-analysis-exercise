source("R/definitions.R")

# Read from months csv and stores it on a data frame
aprilsbs.df <- read.csv(file="data/bad_smells_abril.csv", header=TRUE)
maysbs.df <- read.csv(file="data/bad_smells_maio.csv", header=TRUE)

# 1º Item
analyzeMonth(aprilsbs.df, mname="Abril")
# 2º Item
analyzeMonth(maysbs.df, mname="Maio")