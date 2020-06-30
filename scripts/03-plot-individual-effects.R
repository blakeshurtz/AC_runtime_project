#read in dataset
d <- read_csv("data/ccl.csv")

#read in thresholds (note threshold determined using the POT package)
#see the script "threshold_estimation.R" in dir "scripts/old & misc/"
thresholds <- read_csv("data/customer-thresholds.csv")

d <- d %>%
  left_join(thresholds, by = 'name') %>% 
  filter(ccl >= threshold) %>%  
  select(name, ccl) %>%  
  arrange(name, ccl) 

#create ccdf
d <- d %>% group_by(name) %>% mutate(n = n(),
                                     ccdf = seq(1, 0, length.out = n)) 

###read in model parameters
params <- read.csv("data/indv_params_means.csv")

#generate probabilities for CCL along quantiles for all buildings/systems (function)
generate_probs <- function(i){
  pgpd(seq(from = 0, to = 10, by = 0.01), mu = params$threshold[i], xi = params$k[i], sigma = params$sigma[i], lower.tail = FALSE) 
}

#create NULL matrix
ccl_probs <- matrix(0, nrow = 1001, ncol = nrow(params))
colnames(ccl_probs) <- unique(params$name)

#generate probs for all systems
for (i in 1:nrow(params)){
  ccl_probs[,i] <- generate_probs(i)
}

#gather for tall dataframe
ccl_probs <- gather(as.data.frame(ccl_probs), dplyr::starts_with("customer"), key = "name", value = "ccdf")

#add CCDF
ccl_probs <- ccl_probs %>% group_by(name) %>% mutate(n = n(),
                                                 ccl = seq(from = 0, to = 10, by = 0.01)) %>% 
  arrange(name, desc(ccdf))

#rename from customer ot buildings
ccl_probs$name <- gsub('customer', 'building', ccl_probs$name)
d$name <- gsub('customer', 'building', d$name)

g1 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_01", "building_02")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_01", "building_02")), size = 1, show.legend = FALSE) +
  theme_bw() +
  facet_wrap(vars(name)) +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load")

g2 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_03", "building_04")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_03", "building_04")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load")


g3 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_05", "building_06")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_05", "building_06")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load")

g4 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_07", "building_08")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_07", "building_08")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load")

g5 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_09", "building_10")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_09", "building_10")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load")

g6 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_11", "building_12")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_11", "building_12")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load") 

g7 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_13", "building_14")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_13", "building_14")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load") 

g8 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_15", "building_16")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_15", "building_16")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load")

g9 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_17", "building_18")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_17", "building_18")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load") 

g10 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_19", "building_20")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_19", "building_20")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load")

g11 <- ggplot() + 
  geom_point(aes(ccl, ccdf), color = "red", data = subset(d, name %in% c("building_21", "building_22")), size = 1, show.legend = FALSE) +
  geom_line(aes(ccl, ccdf), color = "blue", data = subset(ccl_probs, name %in% c("building_21", "building_22")), size = 1, show.legend = FALSE) +
  facet_wrap(vars(name)) +
  theme_bw() +
  theme(text = element_text(size = 20), axis.title=element_text(size=14)) +
  scale_x_continuous(breaks=c(seq(from = 1, to = 10, by = 1)), limits=c(0,10)) +
  coord_trans(y="log10") +
  scale_y_continuous(breaks=c(1e-3,1e-2,1e-1,1), limits=c(1e-3,1), labels = scales::percent_format(accuracy = 1)) +
  labs(y = "P(X > x)", x = "Cyclical Cooling Load") 


g <- arrangeGrob(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, ncol = 1)

ggsave(filename = "plots/indv_plot.jpeg", plot = g, device = "jpeg", dpi = 300,
       width = 150, height = 750, units = "mm", limitsize = FALSE)

rm(d); rm(params); 
rm(ccl_probs); rm(thresholds); rm(i); rm(generate_probs);
rm(g1); rm(g2); rm(g3); rm(g4); rm(g5); rm(g6); rm(g7); rm(g8); rm(g9); rm(g10); rm(g11); rm(g)
