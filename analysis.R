# 1º Item
# Read from aprils csv and stores it on a data frame
aprilsbs.df <- read.csv(file="data/bad_smells_abril.csv", header=TRUE)

# 1.a sum of all aprils bad smells 
aprilsbs.sum <- sum(aprilsbs.df[,-1:-2])
paste("O total de bad smells em abril é de", aprilsbs.sum)

# 1.b total average of all aprils bad smells 
aprilsbs.avg <- aprilsbs.sum / ncol(aprilsbs.df[,-1:-2])
paste("A média total me abril de bad smells em todos os softwares é de", aprilsbs.avg)

# 1.c software with highest bad smells count of april
aprilsbs.toppkg <- aprilsbs.df[which.max(rowSums(aprilsbs.df[,-1:-2])), "package_name"]
paste("O software com mais bad smells am abril é o", aprilsbs.toppkg)

# 1.d plot the total amout of bad smells by type
bs.colsums <- colSums(aprilsbs.df[,-1:-2])
ylim <- c(0, 1.3*max(bs.colsums)) # used to scale the yaxis so the highest bar fits it
bs.colsums.plot <- barplot(bs.colsums, main="Total de bad smells por tipo - Abril", ylim = ylim,
        ylab="Quantidade de bad smells", xlab="Tipo de bad smell")
text(x=bs.colsums.plot, y=bs.colsums, label=bs.colsums, pos=3, cex=0.8) # adds text at top of bars

# 1.e Variance and standart deviation of bad smells types
bs.colvars <- apply(aprilsbs.df[,-1:-2], 2, var)
bs.colsds <- apply(aprilsbs.df[,-1:-2], 2, sd)
paste("A variância dos bad smells do mês por tipo são:")
bs.colvars
paste("A variância dos bad smells do mês por tipo são:")
bs.colsds

# 1.f boxplot the bad smells by type
boxplot(aprilsbs.df[,-1:-2], main="Comparação de bad smells por tipo - Abril", xlab="Tipo de bad smell")

