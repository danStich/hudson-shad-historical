library(tidyverse)

milleretal <- data.frame(
  Habitat = rep(c("Deep Water", "Intertidal", "Shallow Water"), 2),
  Timeframe = rev(sort(rep(c("Historic", "Recent"), 3))),
  Percent = c(61, 16, 23, 36, 23, 41)
)

fig2 <- ggplot(milleretal, aes(x = Timeframe, y = Percent,
                       color = Habitat, fill = Habitat)) +
  geom_col() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values = c("gray80", "gray40", "black")) +
  scale_fill_manual(values = c("gray80", "gray40", "black")) +
  ylab("Percent of total habitat") +
  xlab("Habitat type") +
  theme_bw() +
  theme(axis.text = element_text(size = 12, color = "black"),
        axis.title = element_text(size = 12, color = "black"),
        legend.text = element_text(size = 12, color = "black"),
        legend.position = "top")
  

jpeg(filename = "results/Fig2.jpg",
     res = 300, width = 2000,
     height = 1850)

fig2

dev.off()