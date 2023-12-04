# plot to see all behaviours
plot1 <- function(x_var, facet_var = NULL){
  palette = c("#6ACB4F", "#80DFFF", "#13AADD","#0D7496" ,"#CE2A2A","#CDA2AB","#F07131", "#34B380","grey")
  tf(data, grouping = c(x_var, facet_var), beh.cols.full, colMeans) %>% 
    pivot_longer(cols = beh.cols.full , names_to ="type", values_to="time") %>% 
    mutate(type = fct_relevel(type, beh.cols.full))  %>% 
    ggplot(., aes_string(fill = "type", y = "time", x = x_var))+
    geom_bar(position = position_dodge(), stat="identity", color=grey(0.4))+
    scale_fill_manual(values = palette)+
    theme(legend.position="bottom")
}

# plot distributions
hist1 <- function(x, main = deparse(substitute(x))){
  hist(x, breaks = seq(0,600,10), main = main)
}

# plot one variable at a time
plot_var <- function(data, variable){
  means_tank <- aggregate(data[,variable], list(data[,"Tank"]), mean) %>% 
    setNames(.,c("Tank", variable))
  
  ggplot(data, aes_string(y = variable, x = "Tank")) +
    geom_jitter(alpha = .8, width = 0.05, height=0)+
    geom_point(data = means_tank,colour = "red", size = 4, alpha = .5)+
    geom_line(data = means_tank, aes(group=1), colour = "red", alpha = .5) 
}

# plot one variable at a time for each fish
plot_var_fish <- function(data, variable){
  means_tank <- aggregate(data[,variable], list(data[,"Tank"]), mean) %>% 
    setNames(.,c("Tank", variable))
  means_fish <- aggregate(data[,variable], list(data[,"Tank"],data[,"Fish"]), mean) %>% 
    setNames(.,c("Tank", "Fish", variable))

ggplot(means_fish ,aes_string(y = variable, x = "Tank")) +
  geom_line(data = means_tank, aes(group=1), colour = "red", alpha = .5)+
  geom_point(data = data, alpha=.8)+
  geom_point(size = 2, alpha = .5)+
  geom_line(aes(group=Fish))+
  facet_wrap(~Fish)
}

plot_var_fish2 <- function(data, variable){
  means_tank <- aggregate(data[,variable], list(data[,"Tank"]), mean) %>% 
    setNames(.,c("Tank", variable))
  means_fish <- aggregate(data[,variable], list(data[,"Tank"],data[,"Fish"]), mean) %>% 
    setNames(.,c("Tank", "Fish", variable))
  
  ggplot(means_fish ,aes_string(y = variable, x = "Tank")) +
    geom_line(data = means_tank, aes(group=1), colour = "red", lwd = 2)+
    geom_line(aes(group=Fish))
}

# Plot binary
plot_var_binary <- function(data, variable){
  ggplot(data, aes_string(fill = variable, x = "Tank"))+
    geom_bar(stat= "count",color=grey(0.4))+
    scale_fill_manual(values = c( "gray", "springgreen3"))+
    ylab("Count of trials")+
    theme(legend.position = "bottom")
}

# Plot order of tanks
plot_order <- function(data, variable){
  means_tank <- aggregate(data[,variable], list(data[,"Tank"],data[,"Order"]), mean) %>% 
    setNames(.,c("Tank", "Order",variable))
  
  ggplot(data, aes_string(y = variable, x = "Order", group = "Tank", color = "Tank")) +
    geom_jitter(alpha = .8, width = 0.05, height=0)+
    geom_point(data = means_tank, size = 4, alpha = .5)+
    geom_line(data = means_tank , alpha = .5)+
    scale_color_manual(values = rev(c("#440154FF", "#3B528BFF", "#21908CFF", "#5DC863FF", "#FDE725FF")))+
    ggtitle(variable)
}
