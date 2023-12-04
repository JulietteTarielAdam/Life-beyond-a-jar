source("Load-data.R")
master %>% distinct(Tank, Resting.place, Up.Down) %>% arrange(Tank, Resting.place, Up.Down)
master %>% distinct(Tank, Hovering.place, Up.Down) %>% arrange(Tank, Hovering.place, Up.Down)