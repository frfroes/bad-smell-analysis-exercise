analyzeMonth <- function(csvsource, mname){
  # 1º Item
  # Read from aprils csv and stores it on a data frame
  aprilsbs.df <- read.csv(file=csvsource, header=TRUE)
  
  # 1.a sum of all bad smells 
  aprilsbs.sum <- sum(aprilsbs.df[,-1:-2])
  print(paste("O total de bad smells em", mname ,"é de", aprilsbs.sum))
  
  # 1.b total average of all bad smells 
  aprilsbs.avg <- aprilsbs.sum / ncol(aprilsbs.df[,-1:-2])
  print(paste("A média total em", mname, "de bad smells em todos os softwares é de", aprilsbs.avg))
  
  # 1.c software with highest bad smells count
  aprilsbs.toppkg <- aprilsbs.df[which.max(rowSums(aprilsbs.df[,-1:-2])), "package_name"]
  print(paste("O software com mais bad smells em", mname, "é o", aprilsbs.toppkg))
  
  # 1.d plot the total amout of bad smells by type
  bs.colsums <- colSums(aprilsbs.df[,-1:-2])
  ylim <- c(0, 1.3*max(bs.colsums)) # used to scale the yaxis so the highest bar fits it
  bs.colsums.plot <- barplot(bs.colsums, main=paste("Total de bad smells por tipo - ", mname,""), ylim = ylim,
                             ylab="Quantidade de bad smells", xlab="Tipo de bad smell")
  text(x=bs.colsums.plot, y=bs.colsums, label=bs.colsums, pos=3, cex=0.8) # adds text at top of bars
  
  # 1.e Variance and standart deviation of bad smells types
  bs.colvars <- apply(aprilsbs.df[,-1:-2], 2, var)
  bs.colsds <- apply(aprilsbs.df[,-1:-2], 2, sd)
  print(paste("A variância dos bad smells do mês de", mname,"por tipo são:"))
  print(bs.colvars)
  print(paste("A variância dos bad smells do mês de", mname,"por tipo são:"))
  print(bs.colsds)
  
  # 1.f boxplot the bad smells by type
  boxplot(aprilsbs.df[,-1:-2], main=paste("Comparação de bad smells por tipo -", mname), xlab="Tipo de bad smell")
}