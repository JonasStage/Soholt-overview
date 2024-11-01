kortbaggrund <- theme(panel.background = element_blank(),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      axis.text = element_blank(),
                      axis.ticks = element_blank(),
                      legend.text = element_text(size = 14), 
                      plot.title = element_text(hjust = 0.5, size = 14),
                      legend.title = element_text(size = 14),
                      axis.line = element_blank(),
                      panel.border = element_blank(),
                      legend.position = "none")

tema <-   theme(panel.background = element_blank(),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                strip.text = element_text(size = 12),
                legend.text = element_text(size = 14),
                legend.title = element_text(size = 14),
                legend.position = "bottom",
                plot.title = element_text(hjust = 0.5), 
                axis.text = element_text(size = 12),
                axis.title = element_text(size = 14),
                strip.background = element_rect(fill = "white"),
                panel.border = element_rect(colour = "black", fill=NA),
                legend.key = element_rect(fill = "NA",color = "white"),
                legend.background = element_rect(color = "white"),
                axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0), angle = 90),
                axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10))) 

sem <- function(x) sd(x,na.rm=T)/sqrt(length(x[!is.na(x)]))

### Legend background remove 
#guides(color=guide_legend(override.aes=list(fill=NA)))


