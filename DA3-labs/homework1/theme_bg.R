# Import library
library(RColorBrewer)
library(viridis)
library(gridExtra)
library(cowplot)
library(scales)
# library(extrafont)

theme_bg <- function() {
  # The function sets the basic design of the graphs (axis, grids, titles etc.)
  
  #font_import()
  #loadfonts(device = "win")
  
  # Generate color palette
  
  palette <- c("white", "grey90", "black", "blue") # global
  color.background = palette[1]
  color.grid.major = palette[2]
  color.grid.minor = palette[2]
  color.axis.text = palette[3]
  color.axis.title = palette[3]
  color.title = palette[3]
  
  palette_brewer <- brewer.pal("Blues", n=9)
  color.fill <- palette[4]
  color.line <- palette[3]
  
  # Chart elements
  
  theme_bw() +
    
    # Chart region
    
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=palette[3])) +
    
    # Grids
    
    theme(panel.grid.major=element_line(color=color.grid.major,size=.2)) +
    theme(panel.grid.minor=element_line(color=color.grid.major,size=.2)) +
    # theme(axis.ticks=element_blank()) +
    
    # Legend
    
    theme(legend.position="right") +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=9,color=color.axis.title )) +
    
    # Title & axis labels
    
    theme(plot.title=element_text(color=color.title, size=9, vjust=1.25, hjust=0.5, face = "bold")) +
    theme(axis.text.x=element_text(size=9,color=color.axis.text, face = "bold")) +
    theme(axis.text.y=element_text(size=9,color=color.axis.text, face = "bold")) +
    theme(axis.title.x=element_text(size=9,color=color.axis.title, vjust=0, face = "bold")) +
    theme(axis.title.y=element_text(size=9,color=color.axis.title, vjust=1.25, face = "bold")) +
    # theme(text = element_text(family = "Arial")) + 
    
    # Margins
    
    theme(plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"))
}

# -----------------------------------------------------------------------------------------------------------------------
# Define colors

color <- c(viridis_pal(alpha = 1, begin = 0, end = 0.7, direction = 1, option = "D")(4), "#FDE725FF")
color <- color[c(2,4,3,1,5)]
show_col(color)

#color.outline = "#FFFFFF"
color.outline = "grey90"
color.regline = "#000000"
color.statline = "#4D4D4D"
color.stat = "#000000"
color.background = "grey80"
color.fill = "#000000"

myheight_large = 9
mywidth_large = 12
myheight_small = 5.625
mywidth_small = 7.5

# need for knitr::kable
#linesep = if (booktabs) c('', '', '', '', '\\addlinespace') else '\\hline'


# ggsave modify - test
# my.ggsave <- function(filename = default_name(plot), height= 7.5, width= 10, dpi= 1200, units = "cm") {
#   ggsave(filename=filename, height=height, width=width, dpi=dpi, units = units)
# }
# my.ggsave(paste0(output, "F8_hist1notused_R.png"))
# ----------------------------------------------------------------------------------------------------------------------

# This part will be removed

# Define colors
Palette = c("#0000FF", "#ADD8E6", "#FF8C00", "#000000", "#4D4D4D", "#FFFFFF") #blue, lightblue, orange, black, gray30, white


color.fill = "#0000FF"
color.fill2 ="#ADD8E6"
color.fill3 = "#FF8C00"



viridis_colors = c(
  "#440154FF"
  ,"#481467FF"
  ,"#482677FF"
  ,"#453781FF"
  ,"#404788FF"
  ,"#39568CFF"
  ,"#33638DFF"
  ,"#2D708EFF"
  ,"#287D8EFF"
  ,"#238A8DFF"
  ,"#1F968BFF"
  ,"#20A387FF"
  ,"#29AF7FFF"
  ,"#3CBB75FF"
  ,"#55C667FF"
  ,"#73D055FF"
  ,"#95D840FF"
  ,"#B8DE29FF"
  ,"#DCE319FF"
  ,"#FDE725FF"
)




# color <- c(viridis_pal(alpha = 1, begin = 0, end = 0.7, direction = 1, option = "D")(16))
# 
# for (i in 1:16) { 
# 
# print(
# ggplot(data = hotels, aes (x = e, y = (..count..)/sum(..count..))) +
#   geom_histogram(binwidth = 20, color = color.outline, fill = color[i], size = 0.25, alpha = 0.8,  show.legend=F, na.rm=TRUE) +
#   #extra parameters: center=, closed="left"
#   labs(x = "Residuals", y = "Percentage") +
#   scale_x_continuous(limits = c(-100, 300), breaks = seq(-100, 300, by = 100)) +
#   scale_y_continuous(labels = scales::percent_format(accuracy = 5L)) +
#   theme_bg() +
#   background_grid(major = "y", minor = "y")
# )
#   ggsave(path = output,filename=paste("res_hist",i,".png",sep=""), width=10, height=7.5, units = "cm", dpi = 1200)
# }
