analyzeMonth <- function(monthsbs.df, mname){
  
  # Extract numeric matrix from data frame
  monthsbs.matrix = data.matrix(monthsbs.df[,-1])
  
  # a - sum of all bad smells 
  monthsbs.sum <- sum(monthsbs.matrix)
  print(paste("O total de bad smells em", mname ,"é de", monthsbs.sum))
  
  # b - total average of all bad smells 
  print(paste("A média total em", mname, "de bad smells em todos os softwares é de", mean(monthsbs.matrix)))
  print(paste("A média total em", mname, "de bad smells em todos os softwares por tipo de badsmell é de"))
  print(colMeans(monthsbs.matrix))
  
  # c - software with highest bad smells count
  toppkg <- monthsbs.df[which.max(rowSums(monthsbs.matrix)), "package_name"]
  print(paste("O software com mais bad smells em", mname, "é o", unlist(toppkg)))
  
  # d - plot the total amout of bad smells by type
  bs.colsums <- colSums(monthsbs.matrix)
  ylim <- c(0, 1.3*max(bs.colsums)) # used to scale the yaxis so the highest bar fits it
  bs.colsums.plot <- barplot(bs.colsums, main=paste("Total de bad smells por tipo - ", mname,""), ylim = ylim,
                             ylab="Quantidade de bad smells", xlab="Tipo de bad smell")
  text(x=bs.colsums.plot, y=bs.colsums, label=bs.colsums, pos=3, cex=0.8) # adds text at top of bars
  
  # e - Variance and standart deviation of bad smells types
  bs.colvars <- apply(monthsbs.matrix, 2, var)
  bs.colsds <- apply(monthsbs.matrix, 2, sd)
  print(paste("A variância dos bad smells do mês de", mname,"por tipo são:"))
  print(bs.colvars)
  print(paste("A variância dos bad smells do mês de", mname,"por tipo são:"))
  print(bs.colsds)
  
  # f - boxplot the bad smells by type
  boxplot(monthsbs.matrix, main=paste("Comparação de bad smells por tipo -", mname), xlab="Tipo de bad smell")
}

circularStackedPlot <- function(bspkgdf.orig, scalar=1, from=1){
  
  if(nrow(bspkgdf.orig) - (50 * scalar) <= 0) return()
  
  range.from = from
  range.to = 50 * scalar
  bspkgdf.curr <- bspkgdf.orig[bspkgdf.orig$abril >= range.from & bspkgdf.orig$abril < range.to,]
  
  step = 0
  curr.rows <- nrow(bspkgdf.curr)
  while(curr.rows >= step){
    
    stpstrt = 1+step
    stpend = if(60+step > curr.rows) curr.rows else 60+step;
    
    bspkg.df <- bspkgdf.curr[stpstrt:stpend,]
    
    step = step + 60 # increment step
    
    group.stp <- round(nrow(bspkg.df) / 4)
    bspkg.df$group <- as.factor(c(rep('A', group.stp), rep('B', group.stp), rep('C', group.stp), rep('D', nrow(bspkg.df) - group.stp*3)))
    bspkg.df <- bspkg.df[, c('package_name', 'group', 'abril', 'maio')]
    
    scalevec <- c(0, 50, 100, 150, 200) * scalar
    
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
      geom_segment(data=grid_data, aes(x = end, y = scalevec[1], xend = start, yend = scalevec[1]), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = scalevec[2], xend = start, yend = scalevec[2]), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = scalevec[3], xend = start, yend = scalevec[3]), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = scalevec[4], xend = start, yend = scalevec[4]), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      geom_segment(data=grid_data, aes(x = end, y = scalevec[5], xend = start, yend = scalevec[5]), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
      
      # Add text showing the value of each 100/75/50/25 lines
      annotate("text", x = rep(max(bspkg.df$id),5), y = scalevec, label = as.character(scalevec) , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
      
      ylim((-25 * scalar), max(label_data$tot, na.rm=T)) +
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
    
    print(paste("Plotando range:", range.from, "-", (range.to - 1), "pt.", step / 60))
    print(p) # force to plot
  
  }
  
  circularStackedPlot(bspkgdf.orig, scalar=(scalar + 1), from=range.to)
}