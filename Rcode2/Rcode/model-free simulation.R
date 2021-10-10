## clear the console and memory
rm(list = ls())
graphics.off()
cat("\014")

##
library(ggplot2)
library(ggpubr)
set.seed(2020)

## the theme for plotting 
pic_theme <- theme(axis.title = element_text(face="bold", size = 15),
                    axis.text = element_text(face="bold", size = 13),
                    legend.title = element_blank(),
                    legend.text = element_text(face="bold", size = 15),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black", size = 1),
                    axis.ticks = element_line(size = 1),
                    panel.grid.major.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.x = element_blank(),
                    panel.grid.minor.x = element_blank(),
                    text = element_text(family = "sans"),
                    legend.position = c(0.15, 0.95))


## establish a simple binary-choice
mu1 = 1
mu2 = -1

reward = c(mu1,mu2)
ntrials = 100
# adopt simple random strategy
p=0.5
p_choose=c(p,1-p)
choice=rep(0,ntrials)
choice=s_decision=sample(c(0:1), size = 100, replace = T, prob = p_choose)
accumulated_reward1=rep(0,ntrials)
for (t in 1:ntrials){
  tmp_reward=reward[choice[t]+1]
  if (t==1){
    accumulated_reward1[t]=tmp_reward
  }else{
  accumulated_reward1[t]=accumulated_reward1[t-1]+tmp_reward
  }
}
choice=as.data.frame(choice)
accumulated_reward1=as.data.frame(accumulated_reward1)

## plotting 1
pic_choice_trials1 <-
  ggplot(data = choice, aes(x = c(1: ntrials), y = choice)) +
  geom_point(colour = "#CC79A7", size = 1.5, alpha = 1) +
  geom_step(size = 0.25, alpha = 0.4, linetype = 1) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(0, 1), breaks = c(0, 1)) +
  labs(x = "Number of Trials", y = "Choice") +
  pic_theme

pic_accre_trials1 <- 
  ggplot(data = accumulated_reward1, aes(x = c(1: ntrials), y = accumulated_reward1)) + 
  geom_line(size = 0.75, linetype = 1, colour = "#CC79A7") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(x = "Number of Trials", y = "Accumulated Reward") +
  pic_theme

model_free_sim1 <- ggarrange(pic_choice_trials1, pic_accre_trials1, ncol = 2, nrow = 1, labels = c("A", "B"),
                         widths = c(1.25, 1), heights = 1)

ggsave(filename = "./pic/model_free_sim1.png", plot = model_free_sim1, 
       width = 11, height = 5.5)

#adopt WSLS strategy
p_reward1=c(0.8,0.2)
p_reward2=c(0.2,0.8)
p_reward=rbind(p_reward1,p_reward2)
p_reward=as.data.frame(p_reward)
choice=rep(0,ntrials)
accumulated_reward2=rep(0,ntrials)
for (t in 1:ntrials){
  if (t==1){
    choice[t]=sample(c(1:2), size = 1, replace = T, prob = p_choose)
    tmp_reward=sample(c(1,-1), size = 1, replace = T, prob = p_reward[choice[t],])
    accumulated_reward1[t]=tmp_reward
  }else{
    if(tmp_reward>0){
      choice[t]=choice[t-1]
    }else{
      choice[t]=3-choice[t-1]
    }
    tmp_reward=sample(c(1,-1), size = 1, replace = T, prob = p_reward[choice[t],])
    accumulated_reward2[t]=accumulated_reward2[t-1]+tmp_reward
  }
}
choice=as.data.frame(choice)
accumulated_reward2=as.data.frame(accumulated_reward2)

## plotting 2
pic_choice_trials2 <-
  ggplot(data = choice, aes(x = c(1: ntrials), y = choice)) +
  geom_point(colour = "#CC79A7", size = 1.5, alpha = 1) +
  geom_step(size = 0.25, alpha = 0.4, linetype = 1) +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  scale_y_continuous(limits = c(1, 2), breaks = c(1, 2)) +
  labs(x = "Number of Trials", y = "Choice") +
  pic_theme

pic_accre_trials2 <- 
  ggplot(data = accumulated_reward2, aes(x = c(1: ntrials), y = accumulated_reward2)) + 
  geom_line(size = 0.75, linetype = 1, colour = "#CC79A7") +
  scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +
  labs(x = "Number of Trials", y = "Accumulated Reward") +
  pic_theme

model_free_sim2 <- ggarrange(pic_choice_trials2, pic_accre_trials2, ncol = 2, nrow = 1, labels = c("A", "B"),
                             widths = c(1.25, 1), heights = 1)

ggsave(filename = "./pic/model_free_sim2.png", plot = model_free_sim2, 
       width = 11, height = 5.5)
