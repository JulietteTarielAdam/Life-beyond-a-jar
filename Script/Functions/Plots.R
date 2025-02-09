# palettes
library(viridis)
## behaviour
# palette.beh = c("#80DFFF", "#B3DE69", "#BEBADA" ,"#FB8072","#FCCDE5","#FDB462","#80B1D3") 
palette.beh <- viridis_pal(direction = -1, option = "D")(8)[1:7]
### With Sinking.floating
# palette.beh2 = c(palette.beh, "#BC80BD")
palette.beh2 <- viridis_pal(direction = -1, option = "D")(8)
### with Out of View
palette.beh3 = c(palette.beh2,"grey") 

## Fish
# palette.fish <- c(brewer.pal(12, "Set3")[-2],brewer.pal(8, "Dark2")[c(1,8)])
palette.fish <- viridis_pal(option = "H")(13)

## Resting place
#palette_RP <- c("#F3C8AC","#76ad5e","#96824a","#a7aba6", "#A0F1FF")
palette_RP <- viridis_pal(direction = -1, option = "A")(6)[1:5]

## Hovering place
palette_HP <- c("#E3C9B8", "#6eacc4", "#89D7F5", "#d0effb","#6B4921")

## Interaction types
palette_IT <- c("#F8DA7F", "#F3C8AC", "#90D0AA", "#A0F1FF")


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
  levels(data$Tank) <- c(levels(data$Tank),'')
  means_tank <- aggregate(data[,variable], list(data[,"Tank"]), mean) %>% 
    setNames(.,c("Tank", variable))
  means_fish <- aggregate(data[,variable], list(data[,"Tank"],data[,"Fish"]), mean) %>% 
    setNames(.,c("Tank", "Fish", variable)) %>% 
    mutate(label = if_else(Tank == "Barren", as.character(Fish), NA_character_))
  # No Barren data for Merlion
  means_fish <- within(means_fish, label[Tank == "Large" & Fish == "Merlion"] <- "Merlion") 

  ggplot(means_fish ,aes_string(y = variable, x = "Tank", color="Fish")) +
    geom_line(aes(group=Fish), lwd = 1.2) + 
    guides(color="none")+
    scale_color_manual(values=palette.fish)+
    geom_line(data = means_tank, aes(group=1), colour = "black", lwd = 2, linetype = "dashed")+
    geom_label_repel(aes(label = label), na.rm = TRUE, xlim = c(5,6), direction = "y", 
                     size = 3.5, fontface = "bold")+
    scale_x_discrete(drop=FALSE) 
}

# Plot for binary variables
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