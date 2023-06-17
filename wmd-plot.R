#
# USAGE: Rscript --vanilla wmd-plot.R [parameters]
#        Rscript --vanilla wmd-plot.R list
#        Rscript --vanilla wmd-plot.R excess
#        Rscript --vanilla wmd-plot.R SWE FIN DNK NOR ISL
#        Rscript --vanilla wmd-plot.R all
#        Rscript --vanilla wmd-plot.R series SWE NOR FIN DNK
# 
# OR:    source('wmd-plot.R') 
# from within an interactive session (to stay in session).
#
# DATA SOURCE:
#   https://github.com/akarlinsky/world_mortality
#   https://github.com/akarlinsky/world_mortality/blob/main/world_mortality.csv
#   https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv
#

args <- commandArgs(TRUE)

src_name_in_plot <- 'https://github.com/akarlinsky/world_mortality'
data_url_raw <- 'https://raw.githubusercontent.com/akarlinsky/world_mortality/main/world_mortality.csv'
local_data_file <- 'world-mortality.csv'
fig_png_dpi <- 300.0  # Only used for PNG output, not PDF

if (!file.exists(local_data_file)) {
  print(paste(local_data_file, 'not found; trying to download'))
  utils::download.file(data_url_raw, local_data_file)
}

suppressMessages(library(tidyverse))
D <- read_csv(local_data_file)

stopifnot(ncol(D) == 6)
print(paste('data file:', local_data_file, 'has', nrow(D), 'rows'))
print(unique(D[['time_unit']]))

if (length(args) == 1 && args[1] == 'excess') {
  Dpre <- D %>% filter(year <= 2019, time_unit == 'weekly') %>% group_by(iso3c, year) %>% summarise(totals = sum(deaths, na.rm = TRUE), num = n())
  Dpre_rate <- Dpre %>% group_by(iso3c) %>% summarise(tot = sum(totals), ntot = sum(num)) %>% mutate(avgrate = tot / ntot)

  # It might be useful to subdivide the "post" rates into 2020, 2021, and 2022 separately (and pooled)
  # Additionally the 'monthly' rate ratio need to be assembled 

  Dpost <- D %>% filter(year > 2019, time_unit == 'weekly') %>% group_by(iso3c, year) %>% summarise(totals = sum(deaths, na.rm = TRUE), num = n())
  Dpost_rate <- Dpost %>% group_by(iso3c) %>% summarise(tot = sum(totals), ntot = sum(num)) %>% mutate(avgrate = tot / ntot)

  Djoin <- inner_join(Dpre_rate, Dpost_rate, by = 'iso3c') %>% mutate(excess = avgrate.y / avgrate.x)
  Dfinal <- select(Djoin, c(iso3c, excess)) %>% arrange(excess) %>% mutate(prct = 100.0 * (excess - 1.0))

  print(colnames(Djoin))
  print(Dfinal[['iso3c']])
  print(Dfinal[['excess']])

  gg <- ggplot(Dfinal) + 
          geom_col(aes(y = iso3c, x = prct)) + 
          scale_y_discrete(limits = Dfinal$iso3c) +
          xlab('Excess %') +
          ylab('Country code') +
          ggtitle('Excess total mortality (countries with weekly numbers)', subtitle = 'excess = (rate 2020 and after) vs. (rate 2019 and before)')
  
  fname1 <- 'excess-summary.png'
  png(fname1, width = 8.0, height = 16.0, res = fig_png_dpi, units = 'in')
  #pdf(fname1, width = fig_width, height = fig_height)
  print(gg)
  dev.off()

  q('no')
}

if (length(args) >= 1 && args[1] == 'series') {  
  stopifnot(length(args) > 1)
  suppressMessages(library(lubridate))

  # Here args[2..end] should be country ISO codes
  isos <- c()
  for (a in 2:length(args)) {
    isos <- c(isos, args[a])
  }

  print(isos)
  print(now())

  Dser <- D %>% filter(iso3c %in% isos, time_unit == 'weekly') %>% 
                select(iso3c, year, time, deaths) %>% 
                mutate(timestamp = make_date(year) + (time - 1) * weeks(1), country = iso3c)

  # base rates
  Dser_prerate <- filter(Dser, year <= 2019) %>% group_by(country) %>% summarise(base = mean(deaths))
  print(Dser_prerate)

  Dhat <- inner_join(Dser, Dser_prerate, by = 'country') %>% mutate(rate = deaths / base)
  print(Dhat)

  # raw counts
  gg <- ggplot(Dser, aes(x = timestamp, y = deaths, label = country, colour = country)) +
          geom_line(size = 1.0) +
          geom_point(size = 2.0) +
          ylab('weekly deaths (actual count)') +
          xlab('time') +
          ggtitle('Raw numbers for weekly deaths', 
                  subtitle = paste('data source:', src_name_in_plot, '--- generated:', Sys.time()))
  
  fname1 <- 'raw-series-combined.png'
  png(fname1, width = 12.0, height = 8.0, res = fig_png_dpi, units = 'in')
  print(gg)
  dev.off()

  # normalized to base rate
  gg <- ggplot(Dhat, aes(x = timestamp, y = rate, label = country, colour = country)) +
          geom_line(size = 1.0) +
          geom_point(size = 2.0) +
          ylab('weekly deaths (nondimensional)') +
          xlab('time') +
          ggtitle('Weekly deaths normalized to pre-2020 average', 
                  subtitle = paste('data source:', src_name_in_plot, '--- generated:', Sys.time()))
  
  fname1 <- 'normalized-series-combined.png'
  png(fname1, width = 12.0, height = 8.0, res = fig_png_dpi, units = 'in')
  print(gg)
  dev.off()

  # week-of-the-year average 2015-1019
  Dser_pre_weekly <- filter(Dser, year <= 2019) %>% group_by(country, time) %>% summarise(base = mean(deaths))
  print(Dser_pre_weekly)

  gg <- ggplot(Dser_pre_weekly, aes(x = time, y = base, label = country, colour = country)) +
          geom_line(size = 1.0) +
          geom_point(size = 2.0) +
          ylab('average weekly deaths') +
          xlab('week') +
          ggtitle('Pre-2020 average of weekly deaths', 
                  subtitle = paste('data source:', src_name_in_plot, '--- generated:', Sys.time()))
  
  fname1 <- 'pre-2020-weekly-average.png'
  png(fname1, width = 12.0, height = 8.0, res = fig_png_dpi, units = 'in')
  print(gg)
  dev.off()

  # normalization per week instead; to pull out a better signal; "deseasonalized"
  Dhat_seasonal <- inner_join(Dser, Dser_pre_weekly, by = c('country', 'time')) %>% mutate(rate = deaths / base)
  print(Dhat_seasonal)

  gg <- ggplot(Dhat_seasonal, aes(x = timestamp, y = rate, label = country, colour = country)) +
          geom_line(size = 1.0) +
          geom_point(size = 2.0) +
          ylab('weekly deaths / seasonal mean') +
          xlab('time') +
          ggtitle('Weekly deaths normalized to pre-2020 seasonal average', 
                  subtitle = paste('data source:', src_name_in_plot, '--- generated:', Sys.time()))
  
  fname1 <- 'week-normalized-series-combined.png'
  png(fname1, width = 12.0, height = 8.0, res = fig_png_dpi, units = 'in')
  print(gg)
  dev.off()

  gg <- ggplot(Dhat_seasonal, aes(x = timestamp, y = rate, label = country, colour = country)) +
          geom_smooth(method = 'loess', span = 0.050, se = TRUE, size = 2.0) +
          geom_point(size = 2.0, alpha = 0.30) +
          ylab('weekly deaths / seasonal mean') +
          xlab('time') +
          ggtitle('Weekly deaths normalized to pre-2020 seasonal average', 
                  subtitle = paste('data source:', src_name_in_plot, '--- generated:', Sys.time()))
  
  fname1 <- 'week-normalized-series-combined-loess.png'
  png(fname1, width = 12.0, height = 8.0, res = fig_png_dpi, units = 'in')
  print(gg)
  dev.off()

  # TODO: some sort of cumulative version of the above plot..

  q('no')
}

make_filename <- function(T, makePNG) {
  #ctry <- as.character(unique(T['country_name']))
  ctry <- as.character(unique(T['iso3c']))
  return(sprintf('wmd-%s-plot.%s', ctry, ifelse(makePNG, 'png', 'pdf')))
}

makePDF     <- TRUE   # PNGs are always generated, optionally also generate PDFs 
fig_width   <- 10.0
fig_height  <- 6.0

#
# Useful documentation: 
#   https://ggplot2.tidyverse.org/reference/scale_manual.html
#
pltfun <- function(T) {
  ctryname <- as.character(unique(T['country_name']))
  stopifnot(length(ctryname) == 1)
  iso3name <- as.character(unique(T['iso3c']))
  time_unit <- as.character(unique(T['time_unit']))
  stopifnot(length(time_unit) == 1)
  stopifnot(time_unit == 'weekly' || time_unit == 'monthly')
  if (time_unit == 'weekly') {
    xbreaks <- seq(1, 52, 2)
    xlabelstr <- 'week'
  } else {
    stopifnot(time_unit == 'monthly')
    xbreaks <- seq(1, 12, 1)
    xlabelstr <- 'month'
  }
  #cols <- c("2015" = "orange", "2016" = "yellow", "2017" = "magenta", "2018" = "purple", "2019" = "cyan", "2020" = "blue", "2021" = "green", "2022" = "red", "<pre-2020>" = "black")
  cols <- c("2015" = "#440154FF", "2016" = "#3B528BFF", "2017" = "#21908CFF", "2018" = "#5DC863FF", "2019" = "#FDE725FF")
  cols <- c(cols, "2020" = "red")
  cols <- c(cols, "2021" = "magenta")
  cols <- c(cols, "2022" = "purple")
  cols <- c(cols, "<pre-2020>" = "black")
  cols <- c(cols, "2023" = "grey")
  Tpre <- filter(T, year <= 2019)
  Tbase <- Tpre %>% group_by(time) %>% summarise(deaths = mean(deaths, na.rm = TRUE), num = n(), year = '<pre-2020>')
  Tlate <- filter(T, year >= 2020) %>% select(time, year, deaths)
  gg <- ggplot(Tlate, aes(x=time, y=deaths, label=year, colour=year)) +
          geom_line(size = 1.50) +
          geom_point(size = 2) +
          scale_x_continuous(breaks = xbreaks) +
          ylab(paste(time_unit, 'deaths')) +
          xlab(xlabelstr) +
          ggtitle(paste0('Total mortality: ', ctryname, ' (', iso3name, ')'), subtitle = paste('data source:', src_name_in_plot, '--- generated:', Sys.time())) +
          geom_line(data = Tpre, aes(x=time, y=deaths), show.legend = FALSE, size = 1.25, alpha = 0.2500) + # linetype = 'dashed'
          geom_line(data = Tbase, aes(x=time, y=deaths), show.legend = TRUE, size = 1.75, alpha = 0.7500) +
          scale_colour_manual(values = cols)
  return(gg)
}

countryFigure <- function(A, iso3c_str, makePNG) {
  AC <- filter(A, iso3c == iso3c_str)  
  class(AC[['year']]) <- 'character'
  fname1 <- make_filename(AC, makePNG)
  gg1 <- pltfun(AC)
  if (makePNG) {
    png(fname1, width = fig_width, height = fig_height, res = fig_png_dpi, units = 'in')
  } else {
    pdf(fname1, width = fig_width, height = fig_height)
  }
  print(gg1)
  dev.off()
  return(gg1)
}

availCodes <- unique(D[['iso3c']])

if (length(args) == 0) {
  CTYLIST <- c('SWE', 'NOR', 'DNK', 'FIN', 'ISL')
  print('default country code selection:')
  print(CTYLIST)
} else if (length(args) == 1 && args[1] == 'all') {
  CTYLIST <- availCodes
} else if (length(args) == 1 && args[1] == 'list') {
  Mw <- mutate(filter(D, time_unit == 'weekly'), isoplusname = paste(iso3c, country_name))
  print('*** weekly data ***')
  print(unique(Mw[['isoplusname']]))
  Mm <- mutate(filter(D, time_unit == 'monthly'), isoplusname = paste(iso3c, country_name))
  print('*** monthly data ***')
  print(unique(Mm[['isoplusname']]))
  q("no")
} else if (length(args) >= 1) {
  CTYLIST <- args
} else {
  stopifnot(FALSE)
}

for (k in 1:length(CTYLIST)) {
  if (!(CTYLIST[k] %in% availCodes)) {
    print(paste('skipping:', CTYLIST[k], '(not available)'))
  } else {
    print(paste('making:', CTYLIST[k]))
    countryFigure(D, CTYLIST[k], TRUE)
    if (makePDF) {
      countryFigure(D, CTYLIST[k], FALSE)
    }
  }
} 
