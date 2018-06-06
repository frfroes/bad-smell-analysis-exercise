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

# --------------------
bspkg.df <- data.frame(
  package_name=aprilsbs.df$package_name, 
  abril=rowSums(aprilsbs.df[,-1]), 
  maio=rowSums(maysbs.df[,-1])
  )

circularStackedPlot(bspkg.df)

bspkg.df <- bspkg.df[bspkg.df$abril >= 1 & bspkg.df$abril < 50,]
bspkg.df <- bspkg.df[1:60,]
group.stp <- round(nrow(bspkg.df) / 4)
bspkg.df$group <- as.factor(c(rep('A', group.stp), rep('B', group.stp), rep('C', group.stp), rep('D', nrow(bspkg.df) - group.stp*3)))
bspkg.df <- bspkg.df[, c('package_name', 'group', 'abril', 'maio')]

# Transform data in a tidy format (long format)
bspkg.df = bspkg.df %>% tidyr::gather(key = "month", value="value", -c(1,2)) 

# Set a number of 'empty bar' to add at the end of each group
empty_bar=2
nObsType=nlevels(as.factor(bspkg.df$month))
to_add = data.frame( matrix(NA, empty_bar*nlevels(bspkg.df$group)*nObsType , ncol(bspkg.df)) )
colnames(to_add) = colnames(bspkg.df)
to_add$group=rep(levels(bspkg.df$group), each=empty_bar*nObsType )
bspkg.df=rbind(bspkg.df, to_add)
bspkg.df=bspkg.df %>% arrange(group, package_name)
bspkg.df$id=rep( seq(1, nrow(bspkg.df)/nObsType) , each=nObsType)

# Get the name and the y position of each label
label_data= bspkg.df %>% group_by(id, package_name) %>% summarize(tot=sum(value))
number_of_bar=nrow(label_data)
angle= 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust<-ifelse(angle < -90, 1, 0)
label_data$angle<-ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=bspkg.df %>% 
  group_by(group) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# Make the plot
p = ggplot(bspkg.df) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=value, fill=month), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE, name="Mês") +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(bspkg.df$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-25,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(0,4), "cm")
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=package_name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE) +

  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )
  #geom_text(data=base_data, aes(x = title, y = -18, label=group), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
p

