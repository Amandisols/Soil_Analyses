
#modify this to be dynamic sel_elem <- c('Fe', 'Si', 'Ca', 'P')
#stat_box_data <- function(y, upper_limit = 1.5) {
#  return( 
#    data.frame(
#      y = 0.95 * upper_limit,
#      label = paste('count =', length(y), '\n',
#                    'mean =', round(mean(y), 1), '\n')
#    )
#  )
#}

#for mean only:
#stat_box_data <- function(y, upper_limit = 1) {
#  return( 
#    data.frame(
#      y = 0.95 * upper_limit,
#      label = paste('mean =', round(mean(y), 1), '\n')
#    )
#  )
#}

#p1 <- ggplot(soilsub, aes(x = Cat, y = tau_Al, fill = Cat)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[Al])) + stat_summary(
#  fun.data = stat_box_data, 
#  geom = "text", 
#  hjust = 0.5,
#  vjust = 0.9
#) + theme(legend.position = "none") + ggtitle("Soil fine fraction") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=20, face="plain"), axis.title.y = element_text(size=20, face="plain"))

#p <- ggplot(soilsub, aes(x = LatVert, y = TiO2, fill = LatVert)) + geom_violin() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = "TiO2") + stat_summary(
#  fun.data = stat_box_data, 
#  geom = "text", 
#  hjust = 0.5,
#  vjust = 0.9
#) + theme(legend.position = "none") + ggtitle("Soil fine fraction") + theme(plot.title = element_text(size=20,face="bold")) +  theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + ylim(0,1.5) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=20, face="plain"), axis.title.y = element_text(size=20, face="plain"))

#(rocksub$tau_Na2O) * 1.3)
stat_box_data <- function(y, upper_limit = 1.5) {
  return( 
    data.frame(
      y = 0.95 * upper_limit,
      label = paste('count =', length(y), '\n',
                    'mean =', round(mean(y), 1), '\n')
    )
  )
}
rocksub$LatVert <- factor(rocksub$LatVert, levels=c("LE", "LB", "VE", "VB", "C"))
#p <- ggplot(rocksub, aes(x = LatVert, y = TiO2, fill = LatVert)) + geom_violin() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = "[TiO2]") + stat_summary(
#  fun.data = stat_box_data, 
#  geom = "text", 
#  hjust = 0.5,
#  vjust = 0.9
#) + theme(legend.position = "none") + ggtitle("Rock fragment") + theme(plot.title = element_text(size=20,face="bold")) + ylim(0,1.5) + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=15, face="plain"), axis.title.y = element_text(size=15, face="plain"))
p <- ggplot(rocksub, aes(x = LatVert, y = tau_Ca, fill = LatVert)) + geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) + geom_boxplot() + geom_point() + theme_classic() + labs(title = "", x = "Horizon", y = expression(τ[Ca])) + stat_summary(
  fun.data = stat_box_data, 
  geom = "text", 
  hjust = 0.5,
  vjust = 0.9
) + theme(legend.position = "none") + ggtitle("Rock fragment") + theme(plot.title = element_text(size=20,face="bold")) + ylim(-1,1.5) + theme(axis.text.x = element_text(size=12), axis.text.y = element_text(size=12)) + scale_fill_manual(values = c("#cccccc","#90593f","#cccccc","#90593f","#fcff54")) + scale_x_discrete(labels = prettylabs) + theme(axis.title.x = element_text(size=15, face="plain"), axis.title.y = element_text(size=15, face="plain"))
p
ggsave(filename = "ca_rock_violin.png")