# 1º Item
# Read from aprils csv to data frame
aprilsbs.df <- read.csv(file="data/bad_smells_abril.csv", header=TRUE)

# 1.a sum of all aprils bad smells 
aprilsbs.sum <- sum(aprilsbs.df[,-1:-2])
paste("O total de bad smells em abril é de", aprilsbs.sum)

# 1.b total average of all aprils bad smells 
aprilsbs.avg <- aprilsbs.sum / ncol(aprilsbs.df[,-1:-2])
paste("A média total me abril de bad smells em todos os softwares é de", aprilsbs.avg)

aprilsbs.toppkg <- aprilsbs.df[which.max(rowSums(aprilsbs.df[,-1:-2])), "package_name"]
paste("O software com mais bad smells am abril é o", aprilsbs.toppkg)
