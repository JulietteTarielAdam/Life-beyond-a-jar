# palettes
## behaviour
palette.beh = c("#80DFFF", "#B3DE69", "#BEBADA" ,"#FB8072","#FCCDE5","#FDB462","#80B1D3") 
### With Sinking.floating
palette.beh2 = c(palette.beh, "#BC80BD")
### with Out of View
palette.beh3 = c(palette.beh2,"grey") 

## Fish
palette.fish <- c(brewer.pal(12, "Set3")[-2],brewer.pal(8, "Dark2")[c(1,8)])

## Resting place
palette_RP <- c("#F3C8AC", "#A0F1FF", "#6DE481", "#A2794B", "#B8B8B8")

# plot to see all behaviour types
plot1 <- function(x_var, facet_var = NULL, main = data, beh_cols = beh.cols, palette = palette.beh){
  tf(main, grouping = c(x_var, facet_var), beh_cols, colMeans) %>% 
    pivot_longer(cols = beh_cols , names_to ="Behaviour", values_to="time") %>% 
    mutate(Behaviour = fct_relevel(Behaviour, beh_cols))  %>% 
    ggplot(., aes_string(fill = "Behaviour", y = "time", x = x_var))+
    geom_bar(position = position_dodge(), stat="identity", color=grey(0.4))+
    scale_fill_manual(values = palette)+
    theme(legend.position="bottom") +
    ylab("Time spent doing the behaviour (sec)")
}

# plot distribution of the behavioural type
hist1 <- function(x, main = deparse(substitute(x))){
  hist(x, breaks = seq(0,600,10), main = main)
}

# plot one behavioural type by tank
plot_var <- function(data, variable){
  ggplot(data, aes_string(y = variable, x = "Tank")) +
    geom_jitter(alpha = .5, width = 0.05, height=0, color = grey(0.25)) +
    stat_summary(fun.y = mean,geom = "point",colour = "red", size = 4)+
    stat_summary(fun.min = function(x) mean(x) - sd(x), 
                 fun.max = function(x) mean(x) + sd(x), 
                 geom = "errorbar",colour = "red", width = 0.15)
}


# plot one behavioural type by tank and by fish
plot_var_fish <- function(data, variable){
  means_tank <- aggregate(data[,variable], list(data[,"Tank"]), mean) %>% 
    setNames(.,c("Tank", variable))
  means_fish <- aggregate(data[,variable], list(data[,"Tank"],data[,"Fish"]), mean) %>% 
    setNames(.,c("Tank", "Fish", variable)) %>% 
    mutate(label = if_else(Tank == "Barren", as.character(Fish), NA_character_))
  
  ggplot(means_fish ,aes_string(y = variable, x = "Tank", color="Fish")) +
    geom_line(aes(group=Fish), lwd = 1.2) + 
    guides(color="none")+
    scale_color_manual(values=palette.fish)+
    geom_line(data = means_tank, aes(group=1), colour = "black", lwd = 2, linetype = "dashed")+
    geom_label_repel(aes(label = label), na.rm = TRUE)
}

# Plot binary
plot_var_binary <- function(data, variable){
  data2 <- aggregate(data[,variable], 
                     list(data[,"Tank"]), 
                     function(x) (sum(as.integer(x)-1)/length(x))*100)%>% # calculate percentage of trials 
    setNames(.,c("Tank", variable))
  ggplot(data2, aes_string(y= variable,x = "Tank"))+
    geom_bar(stat= "identity",fill=grey(0.8), color = "black")+
    ylab("Percentage of trials")
}

plot_var_binary_fish <- function(data, variable){
  data2 <- aggregate(data[,variable], 
                     list(data[,"Tank"], data[,"Fish"]), 
                     function(x) (sum(as.integer(x)-1)/length(x))*100)%>% 
    setNames(.,c("Tank", "Fish", variable))
  ggplot(data2, aes_string(y= variable,x = "Tank"))+
    geom_bar(stat= "identity",fill=grey(0.8), color = "black")+
    ylab("Percentage of trials")+
    facet_wrap(~Fish)
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
