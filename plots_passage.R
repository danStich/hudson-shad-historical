# Package load ----
library(tidyverse)
library(data.table)

# Results load ----
# . Baseline habitat quality ----
load(file = "results/sim_result.rda")

res <- lapply(result, function(x) x[[c('res')]])
resdf <- data.frame(rbindlist(res))

# Create a better column for dam
resdf$dam <- trimws(substr(resdf$dam_removed, 1, 9))
resdf$dam <- trimws(gsub("LOCK", "", resdf$dam))
resdf$dam <- gsub("-", "", resdf$dam)
resdf$dam <- str_to_sentence(resdf$dam)
resdf$dam <- factor(resdf$dam,
                    levels = c("None", "Troy", "C1", "C2", "C3", "C4", "C5", "C6"))

resdf$quality <- "Baseline (100 fish/acre)"

# . Loss of shallow habitat ----
load(file = "results/sim_result_quality.rda")

resquality <- lapply(result, function(x) x[[c('res')]])
resqualitydf <- data.frame(rbindlist(resquality))

# Create a better column for dam
resqualitydf$dam <- trimws(substr(resqualitydf$dam_removed, 1, 9))
resqualitydf$dam <- trimws(gsub("LOCK", "", resqualitydf$dam))
resqualitydf$dam <- gsub("-", "", resqualitydf$dam)
resqualitydf$dam <- str_to_sentence(resqualitydf$dam)
resqualitydf$dam <- factor(resqualitydf$dam,
                    levels = c("None", "Troy", "C1", "C2", "C3", "C4", "C5", "C6"))

resqualitydf$quality <- "Shallow habitat (44 fish/acre)"

# . Loss of SAV ----
load(file = "results/sim_result_quality_sav_passage.rda")

resqualitysav <- lapply(result, function(x) x[[c('res')]])
resqualitysavdf <- data.frame(rbindlist(resqualitysav))

# Create a better column for dam
resqualitysavdf$dam <- trimws(substr(resqualitysavdf$dam_passed, 1, 9))
resqualitysavdf$dam <- trimws(gsub("LOCK", "", resqualitysavdf$dam))
resqualitysavdf$dam <- gsub("-", "", resqualitysavdf$dam)
resqualitysavdf$dam <- str_to_sentence(resqualitysavdf$dam)
resqualitysavdf$dam <- factor(resqualitysavdf$dam,
                           levels = c("None", "Troy", "C1", "C2", "C3", "C4", "C5", "C6"))

resqualitysavdf$quality <- "Shallow habitat and SAV (25 fish/acre)"


# . Combined results ----
names(resdf) <- gsub("dam_removal", "passage", x = names(resdf))
names(resdf) <- gsub("dam_removed", "dam_passed", x = names(resdf))

names(resqualitydf) <- gsub("dam_removal", "passage", x = names(resqualitydf))
names(resqualitydf) <- gsub("dam_removed", "dam_passed", x = names(resqualitydf))

results <- rbind(resdf, resqualitydf, resqualitysavdf)

results$habitat_scenario[results$habitat_scenario == "Modern"] <- 
  "Post-dredging"
results$habitat_scenario[results$habitat_scenario == "Historical"] <- 
  "Pre-dredging"

# Plotting code ----
# . Summarize results ----
plotter <- results %>% 
  group_by(dam, habitat_scenario, quality) %>% 
  summarize(fit = mean(spawners),
            lwr = quantile(spawners, 0.025),
            Q1 = quantile(spawners, 0.25),
            Q3 = quantile(spawners, 0.75),
            upr = quantile(spawners, 0.975))

# . Make the plot ----
Figure5 <- ggplot(plotter, aes(x = dam, y = fit, 
                  color = habitat_scenario, 
                  fill = habitat_scenario)) +
  geom_point(size = 4, position = position_dodge(.9)) +
  geom_linerange(aes(xmax = dam, ymin = Q1, ymax = Q3),
                 position = position_dodge(.9),
                 linewidth = 2) +  
  geom_linerange(aes(xmax = dam, ymin = lwr, ymax = upr),
                 position = position_dodge(.9),
                 linewidth = 1) +
  xlab("Dam passed") +
  ylab("Millions of spawning adults") +
  labs(color = "Channel state", fill = "Channel state") +
  scale_y_continuous(breaks = seq(0, 7e6, 1e6), labels = seq(0, 7, 1)) +
  scale_color_manual(breaks = c("Pre-dredging", "Post-dredging"),
                     values = c("gray50", "black")) +
  scale_fill_manual(breaks = c("Pre-dredging", "Post-dredging"),
                    values = c("gray50", "black")) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1, size = 12),
    axis.title.y = element_text(vjust = 3, size = 12),
    axis.text = element_text(size = 10),
    legend.position = "top",
    strip.background = element_blank()) +
  facet_wrap(~ quality)

Figure5


# . Print the figure to a file ----
jpeg(filename = "results/Figure5.jpg",
     res = 300,
     width = 2400,
     height = 1800)

Figure5

dev.off()

# Summary statistics scratchpad for report ----

baseline <- plotter %>% 
  filter(quality == "Baseline (100 fish/acre)" &
           habitat_scenario == "Pre-dredging")

baseline <- plotter %>% 
  filter(quality == "Baseline (100 fish/acre)" &
           dam == "None")

baseline <- plotter %>% 
  filter(quality == "Baseline (100 fish/acre)" &
           habitat_scenario == "Post-dredging")

plotter %>% 
  filter(quality == "Shallow habitat (44 fish/acre)" &
           habitat_scenario == "Pre-dredging")

plotter %>% 
  filter(quality == "Shallow habitat and SAV (25 fish/acre)" &
           habitat_scenario == "Pre-dredging")

plotter %>% 
  filter(quality == "Shallow habitat and SAV (25 fish/acre)" &
           habitat_scenario == "Post-dredging")
