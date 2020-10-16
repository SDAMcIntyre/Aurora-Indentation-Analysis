library(tidyverse)
library(patchwork)
library(svglite)

# on ramp not detected properly:
#   Row 10 - 4/Trial 9/--100_1.ddf 
#   Row 9 - 2.2/Trial 1/--100_1.ddf

force_data <- read_delim("../Processed Data/force_data_20200414_184944.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE) %>% 
  filter(!str_detect(sourceFile, '(Row 10 - 4/Trial 9/--100_1\\.ddf)|(Row 9 - 2.2/Trial 1/--100_1\\.ddf)'))
theme_set(theme_bw())

theme_insidelegend <- function(x,y) {
  theme(legend.position = c(x,y), 
        legend.background = element_blank(),
        legend.key = element_rect(colour = 'grey'))}

#### QUALITY CONTROL FIGURES ####

force_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(PercentForceError = (forceChange.mN-targetForce.mN)/targetForce.mN*100,
         targetRampTime.ms = factor(targetRampTime.ms),
         targetForce.mN = factor(targetForce.mN)) %>% 
  ggplot(aes(x = targetForce.mN, y = PercentForceError)) +
  stat_summary(aes(colour = targetRampTime.ms), fun.data = 'mean_cl_normal', geom = 'errorbar',
               position = position_dodge(0.3), width = 0.6, size = 0.6) +
  labs(x = 'Target Force (mN)', y = 'Ramp Force Error (% of Target)', colour = 'Target Ramp Time (ms)',
       title = '% Error in force reached during ramp on period') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.6,0.1) +
  plot_annotation(caption = 'Ramp Force = maximum - minimum force during ramp phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target force/ramp time combination.\nDifferent target ramp times for the same target force are shifted horizontally to avoid overplotting.')
ggsave('../Figures/Force_PeakForceError.svg')

force_data %>% 
  filter(phase == 'hold') %>% 
  mutate(PercentForceError = (heldForce.mN-targetForce.mN)/targetForce.mN*100,
         targetRampTime.ms = factor(targetRampTime.ms),
         targetForce.mN = factor(targetForce.mN)) %>% 
  ggplot(aes(x = targetForce.mN, y = PercentForceError)) +
  stat_summary(aes(colour = targetRampTime.ms), fun.data = 'mean_cl_normal', geom = 'errorbar',
               position = position_dodge(0.3), width = 0.6, size = 0.6) +
  labs(x = 'Target Force (mN)', y = 'Held Force Error (% of Target)', colour = 'Target Ramp Time (ms)',
       title = '% Error in held force') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.5,0.1) +
  plot_annotation(caption = 'Held Force = mean force during hold phase - mean force during pre-stim phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target force/ramp time combination.\nDifferent target ramp times for the same target force are shifted horizontally to avoid overplotting.')
ggsave('../Figures/Force_HoldError.svg')


force_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(PercentRampTimeError = (phaseDuration.ms-targetRampTime.ms)/targetRampTime.ms*100,
         targetRampTime.ms = factor(targetRampTime.ms),
         targetForce.mN = factor(targetForce.mN)) %>% 
  ggplot(aes(x = targetRampTime.ms, y = PercentRampTimeError)) +
  stat_summary(aes(colour = targetForce.mN), fun.data = 'mean_cl_normal', geom = 'errorbar',
               position = position_dodge(0.3), width = 0.6, size = 0.6) +
  labs(colour = 'Target Force (mN)', y = 'Ramp Time Error (% of Target)', x = 'Target Ramp Time (ms)',
       title = '% Error in detected ramp duration') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.5,0.9) +
  plot_annotation(caption = 'Detection of ramp onset and offset was based on a threshold of 10% of the target force rate.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target force/ramp time combination.\nDifferent target ramp times for the same target force are shifted horizontally to avoid overplotting.')
ggsave('../Figures/Force_RampTimeError.svg')


#### Force rate ####

force_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(targetRampTime.ms = factor(targetRampTime.ms),
         targetForce.mN = factor(targetForce.mN)) %>% 
  nest(data = -c(targetRampTime.ms,targetForce.mN)) %>% 
  mutate(mForR = map(data, ~ mean_cl_normal(.x$meanForceRate.mNps/1000)),
         pForR = map(data, ~ mean_cl_normal(.x$peakForceRate.mNps/1000))) %>% 
  unnest(mForR, names_sep = '_') %>% 
  unnest(pForR, names_sep = '_') %>% 
  ggplot(aes(x = mForR_y, y = pForR_y)) +
  geom_errorbar(aes(colour = targetRampTime.ms, ymin = pForR_ymin, ymax = pForR_ymax), size = 0.6, width = 0) +
  geom_errorbarh(aes(colour = targetRampTime.ms, xmin = mForR_ymin, xmax = mForR_ymax), size = 0.6, height = 0) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey') +
  geom_point(aes(colour = targetRampTime.ms), alpha = 0.3, size = 2) +
  scale_x_log10(labels = c(0.01,0.1,1.0,10,100), breaks = c(0.01,0.1,1.0,10,100)) + 
  scale_y_log10(labels = c(0.01,0.1,1.0,10,100), breaks = c(0.01,0.1,1.0,10,100)) +
  labs(x = 'Mean Force Rate (N/sec)', y = 'Peak Force Rate (N/sec)', colour = 'Target Ramp Time (ms)',
       title = 'Mean vs Peak Force Rate') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.6,0.15) +
  plot_annotation(caption = 'Mean and peak force rate during the ramp on phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target force/ramp time combination.')
ggsave('../Figures/Force_Mean_vs_Peak_ForceRate.svg')


#### Velocity ####

force_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(targetRampTime.ms = factor(targetRampTime.ms),
         targetForce.mN = factor(targetForce.mN)) %>% 
  nest(data = -c(targetRampTime.ms,targetForce.mN)) %>% 
  mutate(mVel = map(data, ~ mean_cl_normal(.x$meanVelocity.mmps)),
         pVel = map(data, ~ mean_cl_normal(.x$peakVelocity.mmps))) %>% 
  unnest(mVel, names_sep = '_') %>% 
  unnest(pVel, names_sep = '_') %>% 
  ggplot(aes(x = mVel_y, y = pVel_y)) +
  geom_errorbar(aes(colour = targetRampTime.ms, ymin = pVel_ymin, ymax = pVel_ymax), size = 0.6, width = 0) +
  geom_errorbarh(aes(colour = targetRampTime.ms, xmin = mVel_ymin, xmax = mVel_ymax), size = 0.6, height = 0) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey') +
  geom_point(aes(colour = targetRampTime.ms), alpha = 0.3, size = 2) +
  scale_x_log10(labels = c(0.01,0.1,1.0,10,100,1000), breaks = c(0.01,0.1,1.0,10,100,1000)) + 
  scale_y_log10(labels = c(0.01,0.1,1.0,10,100,1000), breaks = c(0.01,0.1,1.0,10,100,1000)) +
  labs(x = 'Mean Velocity (mm/sec)', y = 'Peak Velocity (mm/sec)', colour = 'Target Ramp Time (ms)',
       title = 'Mean vs Peak Velocity') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.35,0.9) +
  plot_annotation(caption = 'Mean and peak velocity during the ramp on phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target force/ramp time combination.')
ggsave('../Figures/Force_Mean_vs_Peak_Velocity.svg')


#### Psychophysics stim set ####

force_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(targetRampTime.ms = factor(targetRampTime.ms),
         targetForce.mN = factor(targetForce.mN)) %>% 
  nest(data = -c(targetRampTime.ms,targetForce.mN)) %>% 
  mutate(pVel = map(data, ~ mean_cl_normal(.x$peakVelocity.mmps)),
         pFor = map(data, ~ mean_cl_normal(.x$forceChange.mN))) %>% 
  unnest(pVel, names_sep = '_') %>% 
  unnest(pFor, names_sep = '_') %>% 
  ggplot(aes(x = pFor_y, y = pVel_y)) +
  geom_rect(aes(xmin = 95, xmax = 3500, ymin = 9, ymax = 50), fill = NA, colour = 'grey', size = 0.3) +
  geom_rect(aes(xmin = 50, xmax = 500, ymin = 1.4, ymax = 8), fill = NA, colour = 'grey', size = 0.3) +
  geom_errorbar(aes(colour = targetRampTime.ms, ymin = pVel_ymin, ymax = pVel_ymax), size = 0.6, width = 0) +
  geom_errorbarh(aes(colour = targetRampTime.ms, xmin = pFor_ymin, xmax = pFor_ymax), size = 0.6, height = 0) +
  geom_point(aes(colour = targetRampTime.ms), alpha = 0.3, size = 2) +
  scale_x_log10() + scale_y_log10(labels = c(0.1,1.0,10,100,1000), breaks = c(0.1,1.0,10,100,1000)) +
  labs(x = 'Peak Force (mN)', y = 'Peak Velocity (mm/sec)', colour = 'Target Ramp Time (ms)',
       title = 'Peak Force vs. Peak Velocity') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.6,0.15) +
  annotate(geom = 'text', x = c(150,80), y = c(38,6), label = c('A','B'), colour = 'darkgrey') +
  plot_annotation(caption = 'Peak force and velocity during the ramp on phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target force/ramp time combination.\n A and B indicate candidate stimulus sets for a psychophysics experiment with a fast and slow velocity, covering a range of forces.\nA: high velocity 50mm/s, low velocity 9mm/s, forces between 100 and 3000mN.\nB: high velocity 8mm/s, low velocity 1.4mm/s, forces between 50 and 500mN.')
ggsave('../Figures/Force_PeakForce_vs_PeakVelocity.svg')

#### Compare to finger press ####

force_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(targetRampTime.ms = factor(targetRampTime.ms),
         targetForce.mN = factor(targetForce.mN)) %>% 
  nest(data = -c(targetRampTime.ms,targetForce.mN)) %>% 
  mutate(pDisp = map(data, ~ mean_cl_normal(.x$positionChange.mm)),
         mForR = map(data, ~ mean_cl_normal(.x$meanForceRate.mNps/1000))) %>% 
  unnest(pDisp, names_sep = '_') %>% 
  unnest(mForR, names_sep = '_') %>% 
  ggplot(aes(x = mForR_y, y = pDisp_y)) +
  geom_rect(aes(xmin = 3, xmax = 10, ymin = 5, ymax = 9), fill = NA, colour = 'grey') +
  geom_errorbar(aes(colour = targetRampTime.ms, ymin = pDisp_ymin, ymax = pDisp_ymax), size = 0.6, width = 0) +
  geom_errorbarh(aes(colour = targetRampTime.ms, xmin = mForR_ymin, xmax = mForR_ymax), size = 0.6, height = 0) +
  geom_point(aes(colour = targetRampTime.ms), alpha = 0.3, size = 2) +
  scale_x_log10(labels = c(0.01,0.1,1.0,10,100), breaks = c(0.01,0.1,1.0,10,100)) + 
  scale_y_continuous(trans = 'log1p')  +
  labs(x = 'Mean Force Rate (N/sec)', y = 'Peak Displacement (mm)', colour = 'Target Ramp Time (ms)',
       title = 'Peak Displacement vs. Mean Force Rate') +
  theme_insidelegend(0.13,0.57) +
  plot_annotation(caption = 'Peak displacement vs. mean force rate during the ramp on phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target force/ramp time combination.\n The grey box indicates force and displacement ranges observed in another experiment in which people pressed a finger into a plum.')
ggsave('../Figures/Force_PeakDisp_vs_MeanForceRate.svg')

