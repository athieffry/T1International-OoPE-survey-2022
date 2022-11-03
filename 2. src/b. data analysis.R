# T1International 2022 Survey: data analysis ###
# Axel Thieffry - November 2022
library(tidyverse)
library(tidylog)
library(magrittr)
library(reshape2)
library(forcats)
library(RColorBrewer)
library(ggrepel)
library(scales)
'%!in%' <- Negate('%in%')
'h' <- head
'l' <- length



# SETUP ------------------------------------------------------------------------
# working directory
setwd('~/Dropbox/T1International-OoPE-survey-2022')
# display sanity checks
SANITY_CHECK=F
# plots
PLOT=F



# 1. READ CLEANED DATA ---------------------------------------------------------
data <- readRDS('1. data/c. clean/cleaned_data.rds')



# 2. COUNTRIES & HEALTHCARE COVERAGE -------------------------------------------
# a. make country summary dataframe
country_summary <- data %>%
                   select(starts_with('country_')) %>%
                   group_by_all() %>%
                   summarize('n'=n()) %>%
                   ungroup() %>%
                   filter(n > 4) %>%
                   mutate('country_name'=country_name %>% fct_reorder(n),
                          'country_alpha2'=country_alpha2 %>% fct_reorder(n),
                          'top7'=ifelse(n > 25, T, F)) %>%
                   arrange(desc(n))

top7 <- country_summary %>%
        filter(top7) %>%
        pull(country_alpha2)

# b. responses by country (with selection for countries > 25 responses)
  # version 1 - colors indicate kept countries (top7)
  country_summary %>%
    unite('country_name', c('country_name', 'country_alpha2'), sep=' - ') %>%
    mutate('country_name'=country_name %>% fct_reorder(n)) %>%
    ggplot(aes(x=country_name, y=n, fill=top7)) +
           geom_col(lwd=.3, col='black') +
           geom_text(aes(label=n), hjust=-.3) +
           coord_flip() + theme(legend.position='none', aspect.ratio=1) +
           scale_fill_brewer(palette='Dark2', direction=-1) +
           scale_y_continuous(expand=c(0.01, 0, 0.1, 0)) +
           labs(title='Responses by country',
                x=paste0('Country (N=', n_distinct(data$country_alpha2),')'),
                y=paste0('Number of responses (N=', format(nrow(data), big.mark=','), ')'))

  # version 2 - colors indicate country income level and background rectangle for kept countries (top7)
  country_income_levels_colors <- brewer.pal(n=3, name='Dark2')[c(2, 3, 1)]

  country_summary %>%
    unite('country_name', c('country_name', 'country_alpha2'), sep=' - ') %>%
    mutate('country_name'=country_name %>% fct_reorder(n)) %>%
    ggplot(aes(x=country_name, y=n, fill=country_income_class)) +
           annotate(geom='rect', xmin=Inf, xmax=14.5, ymin=-Inf, ymax=Inf, fill='lightgreen', alpha=.5) +
           annotate(geom='rect', xmin=-Inf, xmax=14.5, ymin=-Inf, ymax=Inf, fill='grey', alpha=.5) +
           geom_col(lwd=.3, col='black') +
           geom_text(aes(label=n), hjust=-.3) +
           coord_flip() +
           cowplot::theme_cowplot() + theme(legend.position='bottom', aspect.ratio=1) +
           scale_fill_manual(values=country_income_levels_colors, name='Income level') +
           scale_y_continuous(expand=c(0.01, 0, 0.1, 0)) +
           labs(title='Responses by country',
                x=paste0('Country (N=', n_distinct(data$country_alpha2),')'),
                y=paste0('Number of responses (N=', format(nrow(data), big.mark=','), ')'))

# c. country healthcare coverage (top7)
