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
df_1b <- country_summary %>%
         unite('country_name', c('country_name', 'country_alpha2'), sep=' - ') %>%
         mutate('country_name'=country_name %>% fct_reorder(n))

ggplot(df_1b, aes(x=country_name, y=n, fill=top7)) +
       geom_col(lwd=.3, col='black') +
       geom_text(aes(label=n, col=country_income_class), hjust=-.3, fontface='bold') +
       coord_flip() + cowplot::theme_cowplot() +
       theme(legend.position=c(.7, .25), aspect.ratio=1) +
       scale_fill_brewer(palette='Dark2', direction=-1, name='Selected') +
       scale_color_manual(values=c('brown3', 'black', 'deepskyblue3'), name='Income class') +
       scale_y_continuous(expand=c(0.01, 0, 0.15, 0)) +
       labs(title='Responses by country',
            x=paste0('Country (N=', n_distinct(data$country_alpha2),')'),
            y=paste0('Number of responses (N=', format(nrow(data), big.mark=','), ')'))

# c. country healthcare coverage (top7)
healthcare_coverage_colors <- rev(brewer.pal(n=3, name='Set1'))

data %>%
  filter(country_alpha2 %in% top7, coverage != 'pnta') %>%
  group_by(country_alpha2, coverage) %>%
  summarize('n'=n()) %>%
  mutate('country_alpha2'=factor(country_alpha2, levels=c('SE', 'GB', 'DE', 'CA', 'US', 'PA', 'IN')),
         'coverage'=factor(coverage, levels=rev(c('none', 'partial', 'full')))) %>%
  group_by(country_alpha2) %>%
  mutate('total'=sum(n),
         'prop'=n/total*100) %>%
  ggplot(aes(x=country_alpha2, y=prop, fill=coverage)) +
         geom_col(lwd=.3, col='black', alpha=.8) +
         geom_text(aes(label=n, y=prop), position=position_stack(vjust=.5), col='white') +
         scale_fill_manual(values=healthcare_coverage_colors, name='Healthcare\ncoverage') +
         scale_y_continuous(expand=c(0, 0.01)) +
         cowplot::theme_cowplot() + theme(aspect.ratio=2) +
         labs(title='Healthcare coverage in top 7 countries',
              x='Top 7 most represented countries',
              y='% of responses')

# d. make data subset for Top 7 countries only
data_top7 <- data %>% filter(country_alpha2 %in% top7)



# 3. OUT-OF-POCKET EXPENSES ----------------------------------------------------
# a. custom function: compute sum of OoPEs by category
# necessary as sum of empty vector returns 0, and not NA!
category_sum <- function(df, cols) {
                                    df %<>% select(all_of(cols))
                                    nb_NA <- df %>% is.na() %>% rowSums()
                                    result <- ifelse(nb_NA == length(cols), NA, rowSums(df, na.rm=T))
                                    return(result)
                                   }

# b. compute sum of costs by categories (columns starts with 'usd_sum_')
data_top7 %<>%
  mutate('usd_sum_devices'=category_sum(df=., cols=c('usd_pumpsupplies_month', 'usd_cgm_month')),
         'usd_sum_insulins'=category_sum(df=., cols=c('usd_shortacting_month', 'usd_intlongacting_month', 'usd_mixed_month', 'usd_otheracting_month')),
         'usd_sum_glucagon'=usd_glucagon_shot,
         'usd_sum_pen_needles'=usd_pen_syringes_month,
         'usd_sum_strips'=usd_strip_month)

# b. plot OoPEs per expense category
data_top7 %>%
  select(id, starts_with('usd_sum_')) %>%
  melt(id.vars='id', variable.name='category', value.name='USD') %>%
  mutate('category'=case_when(category == 'usd_sum_devices' ~ 'pump & CGM',
                              category == 'usd_sum_insulins' ~ 'insulin',
                              category == 'usd_sum_pen_needles' ~ 'pen & needles',
                              category == 'usd_sum_glucagon' ~ 'glucagon',
                              category == 'usd_sum_strips' ~ 'test strips') %>%
                    factor(levels=c('test strips', 'glucagon', 'pen & needles', 'insulin', 'pump & CGM'))) %>%
  ggplot(aes(x=category, y=USD+1, fill=category)) +
         geom_violin(draw_quantiles=c(.25, .5, .75), trim=T, scale='count') +
         scale_y_log10(expand=c(0, 0.05, 0, 0)) +
         scale_fill_brewer(palette='Set2', direction=-1) +
         coord_flip() + cowplot::theme_cowplot() +
         theme(legend.position='none', aspect.ratio=.4) +
         labs(y='OoPEs (monthly USD+1, log-scale)', x='', title='per expense category')

# c. per healthcare coverage
data_top7 %>%
  select(coverage, starts_with('usd_sum_')) %>%
  filter(coverage != 'pnta') %>%
  mutate('total_sum'=category_sum(df=., cols=c('usd_sum_devices', 'usd_sum_insulins', 'usd_sum_glucagon',
                                               'usd_sum_pen_needles', 'usd_sum_strips'))) %>%
  ggplot(aes(x=total_sum+1, col=coverage)) +
         geom_density(lwd=1) +
         scale_x_log10(expand=c(0, 0.05, 0, 0)) +
         scale_color_brewer(palette='Set1') +
         cowplot::theme_cowplot() +
         theme(aspect.ratio=.3) +
         labs(title='per healthcare coverage', x='OoPEs (monthly USD+1, log-scale)')

# d. per country
data_top7 %>%
  mutate('total_sum'=category_sum(df=., cols=c('usd_sum_devices', 'usd_sum_insulins', 'usd_sum_glucagon',
                                               'usd_sum_pen_needles', 'usd_sum_strips'))) %>%
  select(country_alpha2, total_sum) %>%
  ggplot(aes(x=total_sum+1, col=country_alpha2)) +
         geom_density(lwd=1) +
         scale_x_log10(expand=c(0, 0.05, 0, 0)) +
         scale_color_brewer(palette='Paired', name='', direction=-1) +
         cowplot::theme_cowplot() + theme(aspect.ratio=.4) +
         labs(title='per country', x='OoPEs (monthly USD+1, log-scale)')

# e. breakdown
data_top7 %>%
  filter(coverage != 'pnta') %>%
  select(country_alpha2, coverage,
         usd_sum_devices, usd_sum_insulins, usd_sum_glucagon, usd_sum_pen_needles, usd_sum_strips) %>%
  melt(id.vars=c('country_alpha2', 'coverage'), variable.name='category', value.name='USD') %>%
  mutate('category'=category %>% str_remove('usd_sum_'),
         'country_alpha2'=factor(country_alpha2, levels=c('SE', 'GB', 'DE', 'CA', 'IN', 'US', 'PA'))) %>%
  ggplot(aes(x=USD+1, fill=coverage)) +
         geom_histogram(lwd=.3, col='black', position=position_stack(), binwidth=.2) +
         scale_x_log10() +
         facet_grid(country_alpha2 ~ category, scales='free_y') +
         cowplot::theme_cowplot() + theme(aspect.ratio=.5, legend.position='bottom') +
         scale_fill_brewer(palette='Set1') +
         labs(title='OoPE breakdown',
              x='OoPEs (monthly USD+1, log-scale)',
              y='Number of responses')
