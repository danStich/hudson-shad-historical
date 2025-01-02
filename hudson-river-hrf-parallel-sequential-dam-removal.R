# Package load ----
library(snowfall)
library(anadrofish)
library(tidyverse)

# Parallel settings ----
# Get number of cores
ncpus <- 8

# Initialize snowfall
sfInit(parallel = TRUE, cpus=ncpus, type="SOCK")

# Wrapper function ----
sim <- function(x){

  # . Habitat data template ----
  hudson_default <- custom_habitat_template(species = "AMS",
                                            built_in = TRUE,
                                            river = "Hudson")
  
  # .. Adding upstream habitat surface areas for C-5 and C-6 dams ----
  hudson_add <- data.frame(
    river = "Hudson",
    region = "SI",
    govt = "NJ",
    lat = 42.93666,
    lon = -73.65333,
    dam_name = c("LOCK C-5", "LOCK C-6"),
    dam_order = c(6, 7),
    Hab_sqkm = c(1.112887, 0.145687)) # Values from shadia R package
  
  # .. Modern Hudson River habitat ----
  hudson_modern <- rbind(hudson_default, hudson_add)
  hudson_modern$dam_name[1] <- "NONE"
  
  # .. Historical Hudson River habitat ----
  # 4.888607 lost below Troy dam
  hudson_historical <- hudson_modern
  hudson_historical$Hab_sqkm[1] <- hudson_historical$Hab_sqkm[1] + 4.888607
  
  # .. Fish passage rate scenarios ----
  passage <- rep(0, nrow(hudson_modern))  
  dam_removal_sample <- sample(1:8, 1, replace = TRUE) 
  dam_removed <- hudson_modern$dam_name[dam_removal_sample]
  passage[1:dam_removal_sample] <- 1
  
  # . Sample scenarios ----
  # .. Habitat scenarios ----
  habitats <- list("Modern" = hudson_modern, 
                   "Historical" = hudson_historical)
  
  habitat_data <- sample(habitats, 1 , replace = TRUE)
  habitat_scenario <- names(habitat_data)
  habitat_data <- habitat_data[[1]]
  
  # . Run model ----
  res <- sim_pop(
    species = "AMS",
    nyears = 50,
    river = "Hudson",
    max_age = NULL,
    nM = NULL,
    fM = 0,
    n_init = runif(1, 1e+06, 10e+06),
    spawnRecruit = NULL,
    eggs = NULL,
    sr = rbeta(1, 100, 100),
    b = 0.3417 + (0.3417 * 0.19),
    s_juvenile = NULL,
    upstream = passage,
    downstream = 1,
    downstream_j = 1,
    output_years = 'last',
    age_structured_output = FALSE,
    sex_specific = TRUE,
    custom_habitat = habitat_data)
  
  # . Define the outputs ----
  res$habitat_scenario <- habitat_scenario
  res$dam_removal_sample <- dam_removal_sample
  res$dam_removed <- dam_removed
  
  retlist <- list(
    res = res) 
  
  return(retlist)    
}  

# Parallel execution ----

# . Load libraries on workers -----
sfLibrary(anadrofish)

# . Distribute to workers -----
# Number of simulations to run
niterations <- 100000

# Run the simulation ----
start <- Sys.time()

result <- sfLapply(1:niterations, sim) 

total_time <- Sys.time()-start
total_time

# . Stop snowfall ----
sfStop()

# Results ----
# 'result' is a list of lists. Save this:
# save(result, file = "sim_result_quality_sav.rda")

# . Extract results dataframes by string and rbind them ----
res <- lapply(result, function(x) x[[c('res')]])
library(data.table)
resdf <- data.frame(rbindlist(res))

# Create a better column for dam
resdf$dam <- trimws(substr(resdf$dam_removed, 1, 9))
resdf$dam <- trimws(gsub("LOCK", "", resdf$dam))
resdf$dam <- gsub("-", "", resdf$dam)
resdf$dam <- str_to_sentence(resdf$dam)
resdf$dam <- factor(resdf$dam,
                      levels = c("None", "Troy", "C1", "C2", "C3", "C4", "C5", "C6"))

# . Preliminary plots ----
# .. Violins and boxes ----
ggplot(resdf, aes(x = dam, y = spawners, 
       color = habitat_scenario, fill = habitat_scenario)) +
  geom_violin(alpha = 0.5) +
  geom_boxplot(alpha = 0.6, width = 0.25,
               position = position_dodge(.9)) +
  stat_summary(fun = "mean", geom = "point", position = position_dodge(0.9),
               size = 4) +
  xlab("Dam removed") +
  ylab("Millions of spawning adults") +
  labs(color = "Estuary habitat", fill = "Estuary habitat") +
  scale_y_continuous(breaks = seq(0, 7e6, 1e6), labels = seq(0, 7, 1)) +
  scale_color_manual(breaks = c("Historical", "Modern"),
                     values = c("gray40", "black")) +
  scale_fill_manual(breaks = c("Historical", "Modern"),
                    values = c("gray40", "black")) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1, size = 14),
    axis.title.y = element_text(vjust = 3, size = 14),
    axis.text = element_text(size = 12))

# .. Lines and ribbons ----
# Summarize and plot results
plotter <- resdf %>% 
  group_by(dam, habitat_scenario) %>% 
  summarize(fit = mean(spawners),
            lwr = quantile(spawners, 0.025),
            upr = quantile(spawners, 0.975))

ggplot(plotter, aes(x = dam, y = fit, group = habitat_scenario,
                  color = habitat_scenario, fill = habitat_scenario)) +
  geom_line() +
  geom_ribbon(aes(xmax = dam, ymin = lwr, ymax = upr, color = NULL), alpha = .5) +
  xlab("Dam removed") +
  ylab("Millions of spawning adults") +
  labs(color = "Estuary habitat", fill = "Estuary habitat") +
  scale_y_continuous(breaks = seq(0, 7e6, 1e6), labels = seq(0, 7, 1)) +
  theme_bw() +
  theme(
    axis.title.x = element_text(vjust = -1, size = 14),
    axis.title.y = element_text(vjust = 3, size = 14),
    axis.text = element_text(size = 12))



