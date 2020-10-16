library(signal)
library(tidyverse)
library(patchwork)
source('aurora functions.R')
theme_set(theme_bw())

dataFolder <- 'C:/Users/sarmc72/OneDrive - Linköpings universitet/projects - in progress/Peripheral speed force/MNG experiment/Aurora data/'
allDataFiles <- list.files(dataFolder, 'ddf', recursive = TRUE)
sortedDataFiles <- tibble(filename = allDataFiles,
                          type = case_when(
                            !str_detect(filename,'(length)|(Velocity)|(bad)|(dont use)|([0-9]v)') ~ 'force',
                            !str_detect(filename,'(force)|(Force)|(bad)|(dont use)') ~ 'length'
                          )
)
lengthDataFiles <- sortedDataFiles %>% 
  dplyr::filter(type == 'length') %>% pull(filename)

outputFolder <- '../Processed Data/'
timenow <- format(Sys.time(), '%Y%m%d_%H%M%S')
outputDataFile <- paste0(outputFolder, 'length_data_',timenow,'.txt')
outputPlotFolder <- paste0(outputFolder,'Length Plots ',timenow,'/')
outputTracesFolder <- paste0(outputFolder,'Length Overlay ',timenow,'/')

overlayData <- tibble()
for (n in seq_along(lengthDataFiles)) {
  # for (n in 1:2) {
  ddfFile <- paste0(dataFolder,lengthDataFiles[n])
  
  print(paste(n, 'of', length(lengthDataFiles), ':', ddfFile))
  
  # read the protocol info
  ptcl <- ddfFile %>% read_length_protocol(skip = 16, n_max = 3)
  
  # read the raw data
  ddfData <- ddfFile %>% read_delim('\t', skip = 23, col_types = paste0(rep('d',12), collapse = ''))
  #ddfData %>% plot_ddf_data()
  
  # make noise filter
  SamplingRate <- read_lines(ddfFile, skip = 1, n_max = 1) %>% parse_number()
  FilterFreqCutOff <- ceiling(10^(1.4 + 0.6*log10( 1000/ptcl$rampOnDuration.ms))) 
  butterworthFilter <- butter(n = 2, W = FilterFreqCutOff/SamplingRate, type = "low")
  
  # data in real units, filtered data, and time derivative
  scaleUnits <- read_lines(ddfFile, skip = 8, n_max = 1) %>% 
    str_split('\t') %>% .[[1]] %>% .[-1] %>% parse_double()
  
  scaledData <- ddfData %>% 
    scale_and_filter(scaleUnits, SamplingRate, butterworthFilter) %>% 
    dplyr::filter(Time.ms %>% between(ptcl$stimWindowStart, ptcl$stimWindowEnd))
  
  # automatically find where the force ramps are
  velocityThreshold <- 0.1*(ptcl$targetLength.mm/ptcl$rampOnDuration.ms)
  lengthRamps <- find_ramps(scaledData, ptcl, LengthDeriv.mps, velocityThreshold)
  
  # save summary data
  summaryData <- scaledData %>%
    summarise_data(ramps = lengthRamps) %>% 
    mutate(targetPosition.mm = ptcl$targetLength.mm,
           targetRampTime.ms = ptcl$rampOnDuration.ms,
           sourceFile = ddfFile %>% str_replace(dataFolder,'')
    )
  
  tare <- summaryData %>% 
    dplyr::filter(phase == 'pre-stim') %>% 
    select(meanForce.mN, meanPosition.mm)
  
  overlayData <- bind_rows(overlayData,
                           scaledData %>% 
                             mutate(Force.mN = ForceFiltered.mN - tare$meanForce.mN,
                                    Length.mm = LengthFiltered.mm - tare$meanPosition.mm) %>% 
                             select(Time.ms, Force.mN, Length.mm) %>% 
                             mutate(targetPosition.mm = ptcl$targetLength.mm,
                                    targetRampTime.ms = ptcl$rampOnDuration.ms,
                                    sourceFile = ddfFile %>% str_replace(dataFolder,'')
                                    )
  )
  
  summaryData %>% 
    write_delim(paste0(outputDataFile), '\t', append = n>1)
  print(paste("added data to", outputDataFile))
  
  # windows()
  # 
  # scaledData %>% 
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = c(ptcl$rampOn,ptcl$hold,ptcl$rampOff,ptcl$endStim), colour = 'grey') +
  #   geom_vline(xintercept = unlist(lengthRamps), colour = 'red') +
  #   geom_point(aes(y = LengthMeasued.mm), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
  #   geom_point(aes(y = LengthFiltered.mm), colour = 'purple', size = 1) +
  #   labs(title = paste('Displacement trace'), 
  #        x = 'Time (ms)', y = 'Displacement (mm)') -> disp.trace
  # 
  # scaledData %>% 
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = unlist(lengthRamps), colour = 'red') +
  #   geom_point(aes(y = LengthDeriv.mps), colour = 'purple', size = 1) +
  #   labs(title = 'Displacement derivative', x = 'Time (ms)', y = 'Velocity (m/s)') -> disp.deriv
  # 
  # scaledData %>% 
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = unlist(lengthRamps), colour = 'red') +
  #   geom_point(aes(y = ForceMeasured.mN), shape = 21, fill = 'black', alpha = 0.1, size = 3) +
  #   geom_point(aes(y = ForceFiltered.mN), colour = 'blue', size = 1) +
  #   labs(title = 'Force trace', x = 'Time (ms)', y = 'Force (mN)') -> force.trace
  # 
  # scaledData %>% 
  #   ggplot(aes(x = Time.ms)) +
  #   geom_vline(xintercept = unlist(lengthRamps), colour = 'red') +
  #   geom_point(aes(y = ForceDeriv.Nps), colour = 'blue', size = 1) +
  #   labs(title = 'Force derivative', x = 'Time (ms)', y = 'Force rate (N/s)') -> force.deriv
  # 
  # 
  # disp.trace / disp.deriv / force.trace / force.deriv + 
  #   plot_annotation(title = paste('Target =', ptcl$targetLength.mm,'mm.',
  #                                 ptcl$rampOnDuration.ms,'ms ramp.', 
  #                                 'Low pass butterworth filter',FilterFreqCutOff,'Hz'))
  # 
  # plotFile <- ddfFile %>%
  #   str_replace(dataFolder,'') %>% 
  #   str_replace_all('/','_') %>% 
  #   paste0(outputPlotFolder,.) %>%
  #   str_replace('ddf', 'tiff')
  # 
  # if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
  # ggsave(plotFile)
  # print(paste('saved figure:',plotFile))
  # 
  # dev.off()
}

overlayData %>% 
  group_by(targetPosition.mm,targetRampTime.ms,sourceFile) %>% 
  tally() %>% 
  xtabs( ~ targetPosition.mm + targetRampTime.ms, .)

ramps <- sort_unique(overlayData$targetRampTime.ms)
positions <- sort_unique(overlayData$targetPosition.mm)

for (rampN in seq_along(ramps)) {
  plotlist = list()
  for (positionN in seq_along(positions)) {
    plotData <- overlayData %>% 
      dplyr::filter(targetRampTime.ms == ramps[rampN] & targetPosition.mm == positions[positionN]) %>% 
      mutate(nfiles = n_distinct(sourceFile),
             targetForceLabel = paste0('t=',targetPosition.mm,'mm, n=',nfiles))
    
    if (nrow(plotData) > 0) {
      pos.trace <- plotData %>%
        ggplot(aes(x = Time.ms/1000, y = Length.mm)) +
        facet_wrap( ~ targetForceLabel) +
        geom_line(aes(group = sourceFile), size = 0.5, alpha = 0.4) +
        labs(x = NULL, y = NULL)
      if (positionN==1) pos.trace <- pos.trace + labs(y = 'Position (mm)')
      
      force.trace <- plotData %>%
        ggplot(aes(x = Time.ms/1000, y = Force.mN)) +
        facet_wrap( ~ targetForceLabel) +
        geom_line(aes(group = sourceFile), size = 0.5, alpha = 0.4) +
        labs(x = 'Time (sec)', y = NULL)
      if (positionN==1) force.trace <- force.trace + labs(y = 'Force (mN)') 
    
     plotlist[[positionN]] = (pos.trace / force.trace)
    }
  }
  
  windows(17.5,4.5)
  wrap_plots(plotlist, ncol = length(positions)) + 
    plot_annotation(title = paste0('Ramp = ',ramps[rampN],'ms'),
                    caption = paste0('Overlaid position traces (top) and force traces (bottom) for different target positions (columns) with ramp time = ',ramps[rampN],'ms'))
  
  plotFile <- paste0(outputTracesFolder,'Length overlay ',ramps[rampN],'ms ramp.tiff')
  if (!dir.exists(file.path(dirname(plotFile))) ) dir.create(file.path(dirname(plotFile)), recursive = TRUE)
  ggsave(plotFile)
  dev.off()
}

