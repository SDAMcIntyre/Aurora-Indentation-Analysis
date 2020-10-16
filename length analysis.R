library(tidyverse)
library(patchwork)
library(svglite)

# on ramp not detected properly:

length_data <- read_delim("../Processed Data/length_data_20200925_090149.txt", 
                         "\t", escape_double = FALSE, trim_ws = TRUE) 
theme_set(theme_bw())

theme_insidelegend <- function(x,y) {
  theme(legend.position = c(x,y), 
        legend.background = element_blank(),
        legend.key = element_rect(colour = 'grey'))}

#### QUALITY CONTROL FIGURES ####

length_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(PercentRampTimeError = (phaseDuration.ms-targetRampTime.ms)/targetRampTime.ms*100,
         targetRampTime.ms = factor(targetRampTime.ms),
         targetPosition.mm = factor(targetPosition.mm)) %>% 
  ggplot(aes(x = targetRampTime.ms, y = PercentRampTimeError)) +
  stat_summary(aes(colour = targetPosition.mm), fun.data = 'mean_cl_normal', geom = 'errorbar',
               position = position_dodge(0.3), width = 0.6, size = 0.6) +
  scale_y_continuous(trans = 'log1p', breaks = c(1:4,10,20,30,40)) +
  labs(colour = 'Target Displacement (mm)', y = 'Ramp Time Error (% of Target)', x = 'Target Ramp Time (ms)',
       title = '% Error in detected ramp duration') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.5,0.9) +
  plot_annotation(caption = 'Detection of ramp onset and offset was based on a threshold of 10% of the target velocity.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target displacement/ramp time combination.\nDifferent target ramp times for the same target displacement are shifted horizontally to avoid overplotting.')
ggsave('../Figures/Length_RampTimeError.svg')


length_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(PercentDisplacementError = (positionChange.mm-targetPosition.mm)/targetPosition.mm*100,
         targetRampTime.ms = factor(targetRampTime.ms),
         targetPosition.mm = factor(targetPosition.mm)) %>% 
  ggplot(aes(x = targetPosition.mm, y = PercentDisplacementError)) +
  stat_summary(aes(colour = targetRampTime.ms), fun.data = 'mean_cl_normal', geom = 'errorbar',
               position = position_dodge(0.3), width = 0.6, size = 0.6) +
  labs(x = 'Target Displacement (mm)', y = 'Ramp Displacement Error (% of Target)', colour = 'Target Ramp Time (ms)',
       title = '% Error in displacement reached during ramp on period') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.6,0.1) +
  plot_annotation(caption = 'Ramp Displacement = maximum - minimum position during ramp phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target displacement/ramp time combination.\nDifferent target ramp times for the same target discplacement are shifted horizontally to avoid overplotting.')
ggsave('../Figures/Length_PeakDisplacementError.svg')

length_data %>% 
  filter(phase == 'hold') %>% 
  mutate(PercentDisplacementError = (heldPosition.mm-targetPosition.mm)/targetPosition.mm*100,
         targetRampTime.ms = factor(targetRampTime.ms),
         targetPosition.mm = factor(targetPosition.mm)) %>% 
  ggplot(aes(x = targetPosition.mm, y = PercentDisplacementError)) +
  stat_summary(aes(colour = targetRampTime.ms), fun.data = 'mean_cl_normal', geom = 'errorbar',
               position = position_dodge(0.3), width = 0.6, size = 0.6) +
  labs(x = 'Target Displacement (mm)', y = 'Held Displacement Error (% of Target)', colour = 'Target Ramp Time (ms)',
       title = '% Error in held displacement') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.5,0.1) +
  plot_annotation(caption = 'Held Displacement = mean position during hold phase - mean position during pre-stim phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target displacement/ramp time combination.\nDifferent target ramp times for the same target displacement are shifted horizontally to avoid overplotting.')
ggsave('../Figures/Length_HoldError.svg')


#### Force rate ####

length_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(targetRampTime.ms = factor(targetRampTime.ms),
         targetPosition.mm = factor(targetPosition.mm)) %>% 
  nest(data = -c(targetRampTime.ms,targetPosition.mm)) %>% 
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
  scale_y_log10(labels = c(0.01,0.1,1.0,10,100,1000), breaks = c(0.01,0.1,1.0,10,100,1000)) +
  labs(x = 'Mean Force Rate (N/sec)', y = 'Peak Force Rate (N/sec)', colour = 'Target Ramp Time (ms)',
       title = 'Mean vs Peak Force Rate') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.4,0.85) +
  plot_annotation(caption = 'Mean and peak force rate during the ramp on phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target displacement/ramp time combination.')
ggsave('../Figures/Length_Mean_vs_Peak_ForceRate.svg')


#### Velocity ####

length_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(targetRampTime.ms = factor(targetRampTime.ms),
         targetPosition.mm = factor(targetPosition.mm)) %>% 
  nest(data = -c(targetRampTime.ms,targetPosition.mm)) %>% 
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
  plot_annotation(caption = 'Mean and peak velocity during the ramp on phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target displacement/ramp time combination.')
ggsave('../Figures/Length_Mean_vs_Peak_Velocity.svg')



length_data %>% 
  filter(phase == 'ramp on') %>% 
  mutate(targetRampVelocity.mmps = targetPosition.mm/(0.001*targetRampTime.ms),
         targetRampTime.ms = factor(targetRampTime.ms),
         targetPosition.mm = factor(targetPosition.mm)) %>% 
  nest(data = -c(targetRampVelocity.mmps,targetRampTime.ms,targetPosition.mm)) %>% 
  mutate(mVel = map(data, ~ mean_cl_normal(.x$meanVelocity.mmps)),
         pVel = map(data, ~ mean_cl_normal(.x$peakVelocity.mmps))) %>% 
  unnest(mVel, names_sep = '_') %>% 
  unnest(pVel, names_sep = '_') %>% 
  ggplot(aes(x = targetRampVelocity.mmps, y = pVel_y)) +
  geom_errorbar(aes(colour = targetRampTime.ms, ymin = pVel_ymin, ymax = pVel_ymax), size = 0.6, width = 0) +
  geom_errorbarh(aes(colour = targetRampTime.ms, xmin = mVel_ymin, xmax = mVel_ymax), size = 0.6, height = 0) +
  geom_abline(intercept = 0, slope = 1, colour = 'grey') +
  geom_point(aes(colour = targetRampTime.ms), alpha = 0.3, size = 2) +
  scale_x_log10(labels = c(0.01,0.1,1.0,10,100,1000), breaks = c(0.01,0.1,1.0,10,100,1000)) + 
  scale_y_log10(labels = c(0.01,0.1,1.0,10,100,1000), breaks = c(0.01,0.1,1.0,10,100,1000)) +
  labs(x = 'Target Velocity (mm/sec)', y = 'Peak Velocity (mm/sec)', colour = 'Target Ramp Time (ms)',
       title = 'Target vs Peak Velocity') +
  theme(legend.direction = 'horizontal') + theme_insidelegend(0.35,0.9) +
  plot_annotation(caption = 'Peak velocity during the ramp on phase.\nError bars are 95% confidence interval based on the standard error of the mean. N = 10 for each target displacement/ramp time combination.')
ggsave('../Figures/Length_Target_vs_Peak_Velocity.svg')
