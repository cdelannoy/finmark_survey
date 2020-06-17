create_dial <- function(plot_data, section_num){
    
  stopifnot(section_num %in% c("1", "2", "3"))
  data <- filter(plot_data, sectiona == section_num)
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$country), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$country <- rep(levels(data$country), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(country) %>% mutate(Est = estimate*100)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(country) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=Est, fill=country)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    # geom_bar(aes(x=as.factor(id), y=Est, fill=country), stat="identity", alpha=0.5) +
    geom_bar(aes(x=as.factor(id), y=Est, fill=country, alpha = sectionb), stat="identity") +
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20%", "40%", "60%", "80%") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    geom_bar(aes(x=as.factor(id), y=Est, fill=country, alpha = sectionb), stat="identity") +#, alpha=0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      # legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    labs(fill = "Country") +
    scale_alpha_manual(values=c("a"=1,"b"=0.35,"c"=0.1)) +
    guides(alpha = FALSE) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=estimate, label=outcome, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE )
    
    # Add base line information
    # geom_segment(data=base_data, aes(x = start, y = -100, xend = end, yend = -100), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    # geom_text(data=base_data, aes(x = title, y = -18, label=country), hjust=c(1,1,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  p
  
  ggsave(file.path(dir, paste0("Circular_graph_section", section_num, ".png")))
  
  p
}

create_dial2 <- function(plot_data, section_num){
  
  stopifnot(section_num %in% c("1", "2", "3"))
  data <- filter(plot_data, sectiona == section_num) %>% 
    mutate(country = factor(substr(country, 1, 1)))
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$country), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$country <- rep(levels(data$country), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(country) %>% mutate(Est = estimate*100)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(country) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=Est, fill=sectionb)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=Est, fill=sectionb), stat="identity", alpha=0.5) +
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20%", "40%", "60%", "80%") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    geom_bar(aes(x=as.factor(id), y=Est, fill=sectionb), stat="identity", alpha=0.5) +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    scale_fill_brewer(palette="Dark2") +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=estimate, label=outcome, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -100, xend = end, yend = -100), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=country), hjust=c(1,1,0,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
  
  p
  
  ggsave(file.path(dir, paste0("Circular_graph2_section", section_num, ".png")))
  
  p
}

create_dial3 <- function(plot_data, section_num){
  
  stopifnot(section_num %in% c("1", "2", "3"))
  data <- filter(plot_data, sectiona == section_num) %>% 
    mutate(country = factor(substr(country, 1, 1)))
  
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 3
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$country), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$country <- rep(levels(data$country), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(country) %>% mutate(Est = estimate*100)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(country) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  # Make the plot
  p <- ggplot(data, aes(x=as.factor(id), y=Est, fill=outcome)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=Est, fill=outcome), stat="identity") +
    # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
    geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    
    # Add text showing the value of each 100/75/50/25 lines
    annotate("text", x = rep(max(data$id),4), y = c(20, 40, 60, 80), label = c("20%", "40%", "60%", "80%") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    geom_bar(aes(x=as.factor(id), y=Est, fill=outcome), stat="identity") +
    ylim(-100,120) +
    theme_minimal() +
    theme(
      #legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "cm") 
    ) +
    # scale_fill_brewer(palette="Dark2") +
    scale_fill_manual(name = "Legend",
                      values = c(
                        "Borrowed recently" = alpha("darkorchid4", 1),
                        "Missed repaying loan" = alpha("mediumorchid1", 0.6),
                        "No travel last 14 days" = alpha("chartreuse4", 1),
                        "Stay at home" = alpha("olivedrab4", 0.6),
                        "Avoid big groups" = alpha("darkseagreen", 0.3),
                        "Wear face mask" = alpha("darkorange4", 1),
                        "More handsanitizer" = alpha("darkorange2", 0.8),
                        "Seek med care if sympts" = alpha("orange1", 0.5),
                        "Cover mouth" = alpha("goldenrod2", 0.2),
                        "No medicine" = alpha("firebrick4", 1),
                        "Medicine costs more" = alpha("lightcoral", 0.7),
                        "No access to finance" = alpha("steelblue3", 1),
                        "No send/receive money" = alpha("lightblue4", 0.7),
                        "Travel restricted" = alpha("goldenrod3", 1),
                        "Reduced income" = alpha("cyan4", 0.6),
                        "Less food availability" = alpha("brown4", 1),
                        "Not eat > 2 meals/week" = alpha("brown1", 0.6)),
                      breaks = c("Borrowed recently",
                                 "Missed repaying loan",
                                 "No travel last 14 days",
                                 "Stay at home",
                                 "Avoid big groups",
                                 "Wear face mask",
                                 "More handsanitizer",
                                 "Seek med care if sympts",
                                 "Cover mouth",
                                 "No medicine",
                                 "Medicine costs more",
                                 "No access to finance",
                                 "No send/receive money",
                                 "Travel restricted",
                                 "Reduced income",
                                 "Less food availability",
                                 "Not eat > 2 meals/week")) +
    coord_polar() + 
    #geom_text(data=label_data, aes(x=id, y=estimate, label=outcome, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
    
    # Add base line information
    geom_segment(data=base_data, aes(x = start, y = -100, xend = end, yend = -100), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
    geom_text(data=base_data, aes(x = title, y = -18, label=country), hjust=c(1,1,0,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) +
    guides(color=guide_legend(), size = guide_legend())
  
  p
  
  ggsave(file.path(dir, paste0("Circular_graph3_section", section_num, ".png")))
  
  p
}
# create_dial <- function(section_num, plot_data){
#   
#   stopifnot(section_num %in% c("1", "2", "3"))
#   dat <- filter(plot_data, sectiona == section_num)
#   
#   # Set a number of 'empty bar' to add at the end of each group
#   empty_bar <- 4
#   to_add <- data.frame(matrix(NA, empty_bar*n_distinct(dat$country), ncol(dat)))
#   colnames(to_add) <- colnames(dat)
#   to_add$sectiona   <- rep(unique(dat$country), each=empty_bar)
#   data <- rbind(dat, to_add) %>% 
#     arrange(country) %>% 
#     mutate(Est = estimate*100)
#   data$id <- seq(1, nrow(data))
#   
#   # Get the name and the y position of each label
#   label_data <- data
#   number_of_bar <- nrow(label_data)
#   angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
#   label_data$hjust <- ifelse( angle < -90, 1, 0)
#   label_data$angle <- ifelse(angle < -90, angle+180, angle)
#   
#   # prepare a data frame for base lines
#   base_data <- data %>%
#     group_by(country) %>%
#     summarize(start=min(id), end=max(id) - empty_bar) %>%
#     rowwise() %>%
#     mutate(title=mean(c(start, end)))
#   
#   # Make the plot
#   p <- ggplot(data, aes(x=as.factor(id), y=Est, fill=country)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
#     geom_bar(stat="identity", alpha = 0.5) +
#     scale_fill_manual(values = c("deepskyblue1", "deepskyblue4", "darkorange1", "springgreen4")) +
#     ylim(-70,90) +
#     theme_minimal() +
#     theme(
#       legend.position = "none",
#       axis.text = element_blank(),
#       axis.title = element_blank(),
#       panel.grid = element_blank(),
#       plot.margin = unit(rep(-1,4), "cm") 
#     ) +
#     coord_polar() + 
#     geom_text(data=label_data, aes(x=id, y=estimate+10, label=outcome, hjust=hjust), 
#               color="black", fontface="bold",alpha=0.6, size=3, angle= label_data$angle, inherit.aes = FALSE ) +
#   # Add base line information
#   geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
#   geom_text(data=base_data, aes(x = title, y = -18, label=country), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)
#   p
#   
#   ggsave(file.path(dir, country, paste0(country, "_circular_graph_", section_num, ".png")))
#   
#   p
# }