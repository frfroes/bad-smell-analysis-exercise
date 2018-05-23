
# 1º Item
# Read from aprils csv to data frame
aprilsbs.df <- read.csv(file="data/bad_smells_abril.csv", header=TRUE)

# 1.a sum of all aprils bad smells 
aprilsbs.sum <- sum(bsabril.df[,-1:-2])
aprilsbs.sum