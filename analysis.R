# Uncomment to install dependencies
# install.packages("tidyverse", dependencies = TRUE)
# install.packages("viridis", dependencies = TRUE)
# install.packages("ggplot2", dependencies = TRUE)

library(tidyverse)
library(viridis)
library(ggplot2)

source("R/definitions.R")

# Read from months csv and stores it on a data frame
aprilsbs.df <- read.csv(file="data/bad_smells_abril.csv", header=TRUE)
maysbs.df <- read.csv(file="data/bad_smells_maio.csv", header=TRUE)

# Group each row by package and sums their respective collumns values
aprilsbs.df <- aprilsbs.df %>% group_by(package_name) %>% summarise(
  BLOB=sum(BLOB),LM=sum(LM), SAK=sum(SAK), CC=sum(CC), IGS=sum(IGS), MIM=sum(MIM), NLMR=sum(NLMR), LIC=sum(LIC)
)

maysbs.df <- maysbs.df %>% group_by(package_name) %>% summarise(
  BLOB=sum(BLOB),LM=sum(LM), SAK=sum(SAK), CC=sum(CC), IGS=sum(IGS), MIM=sum(MIM), NLMR=sum(NLMR), LIC=sum(LIC)
)

# 1º Item
analyzeMonth(aprilsbs.df, mname="Abril")
# 2º Item
analyzeMonth(maysbs.df, mname="Maio")

#3 º Item
# binds the two data frames into a single labeled one
bscomp.df <- rbind(colSums(aprilsbs.df[,-1]), colSums(maysbs.df[,-1]))
rownames(bscomp.df) <- c("Abril", "Maio")

# a - Plot the comparison of total amout of bad smells by type
ylim <- c(0, 1.3*max(bscomp.df)) # used to scale the yaxis so the highest bar fits it
bscomp.plot <- barplot(bscomp.df, beside=TRUE, legend=rownames(bscomp.df),  ylim = ylim,
        main="Comparação do total de bad smells", ylab="Quantidade de bad smells", 
        xlab="Tipo de bad smell")
text(x=bscomp.plot, y=bscomp.df, label=bscomp.df, pos=3, cex=0.8) # adds text at top of bars

# b - The percetage relation of the amount of bad smells in the two months
print("O a diferença da quantidade de bad smells no mês de Maio em relação a Abril em % por tipo foi de:")
round(((bscomp.df["Maio",] / bscomp.df["Abril",]) - 1) * 100, 2)
print("E a diferença total em % é de:")
round((( sum(bscomp.df["Maio",]) / sum(bscomp.df["Abril",])) - 1) * 100, 2)

# c - Several circular stacked barplot with the comparison of badsemells by package
bspkg.df <- data.frame(
  package_name=aprilsbs.df$package_name, 
  abril=rowSums(aprilsbs.df[,-1]), 
  maio=rowSums(maysbs.df[,-1])
  )

circularStackedPlot(bspkg.df)

