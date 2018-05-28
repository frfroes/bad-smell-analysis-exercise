source("R/definitions.R")

# Read from months csv and stores it on a data frame
aprilsbs.df <- read.csv(file="data/bad_smells_abril.csv", header=TRUE)
maysbs.df <- read.csv(file="data/bad_smells_maio.csv", header=TRUE)

# 1º Item
analyzeMonth(aprilsbs.df, mname="Abril")
# 2º Item
analyzeMonth(maysbs.df, mname="Maio")

bscomp.df <- rbind(colSums(aprilsbs.df[,-1:-2]), colSums(maysbs.df[,-1:-2]))
rownames(bscomp.df) <- c("Abril", "Maio")

#3 º Item
# a - Plot the comparison of total amout of bad smells by type
ylim <- c(0, 1.3*max(bscomp.df)) # used to scale the yaxis so the highest bar fits it
bscomp.plot <- barplot(bscomp.df, beside=TRUE, legend=rownames(bscomp.df),  ylim = ylim,
        main="Comparação do total de bad smells", ylab="Quantidade de bad smells", 
        xlab="Tipo de bad smell")
text(x=bscomp.plot, y=bscomp.df, label=bscomp.df, pos=3, cex=0.8) # adds text at top of bars


