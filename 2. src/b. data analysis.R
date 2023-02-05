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
'at' <- as_tibble
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
## a. make country summary dataframe ####
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

## b. responses by country (with selection for countries > 25 responses) ####
df_1b <- country_summary %>%
         unite('country_name', c('country_name', 'country_alpha2'), sep=' - ') %>%
         mutate('country_name'=country_name %>% fct_reorder(n))

if(PLOT) {
  ggplot(df_1b, aes(x=country_name, y=n, fill=top7)) +
       geom_col(linewidth=.3, col='black') +
       geom_text(aes(label=n, col=country_income_class), hjust=-.3, fontface='bold') +
       coord_flip() + cowplot::theme_cowplot() +
       theme(legend.position=c(.7, .25), aspect.ratio=1) +
       scale_fill_brewer(palette='Dark2', direction=-1, name='Selected') +
       scale_color_manual(values=c('brown3', 'black', 'deepskyblue3'), name='Income class') +
       scale_y_continuous(expand=c(0.01, 0, 0.15, 0)) +
       labs(title='Responses by country',
            x=paste0('Country (N=', n_distinct(data$country_alpha2),')'),
            y=paste0('Number of responses (N=', format(nrow(data), big.mark=','), ')'))
  }

## c. country healthcare coverage (top7) ####
healthcare_coverage_colors <- rev(brewer.pal(n=3, name='Set1'))

if(PLOT) {
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
  }

## d. make data subset for Top 7 countries only ####
data_top7 <- data %>% filter(country_alpha2 %in% top7)



# 3. OUT-OF-POCKET EXPENSES ----------------------------------------------------
## a. custom function: compute sum of OoPEs by category ####
# necessary as sum of empty vector returns 0, and not NA!
category_sum <- function(df, cols) {
                                    df %<>% select(all_of(cols))
                                    nb_NA <- df %>% is.na() %>% rowSums()
                                    result <- ifelse(nb_NA == length(cols), NA, rowSums(df, na.rm=T))
                                    return(result)
                                   }

## b. compute sum of costs by categories (columns starts with 'usd_sum_') ####
data_top7 %<>%
  mutate('usd_sum_devices'=category_sum(df=., cols=c('usd_pumpsupplies_month', 'usd_cgm_month')),
         'usd_sum_insulins'=category_sum(df=., cols=c('usd_shortacting_month', 'usd_intlongacting_month', 'usd_mixed_month', 'usd_otheracting_month')),
         'usd_sum_glucagon'=usd_glucagon_shot,
         'usd_sum_pen_needles'=usd_pen_syringes_month,
         'usd_sum_strips'=usd_strip_month)

## c. plot OoPEs per expense category ####
data_top7 %>%
  select(starts_with('usd_sum_')) %>%
  set_colnames(colnames(.) %>% str_remove('usd_sum_')) %>%
  melt() %>%
  group_by(variable) %>%
  summarize('N'=n(),
            'NAs'=paste0(as.character(sum(is.na(value))), ' (', as.character(round(sum(is.na(value))/n()*100, 1)), '%)'),
            'mean'=mean(value, na.rm=T) %>% round(1),
            'median'=median(value, na.rm=T) %>% round(1),
            'sd'=sd(value, na.rm=T) %>% round(1)) %>%
  gridExtra::grid.table(rows=NULL)

if(PLOT) {
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
  }

## e. per healthcare coverage ####
df_3e <- data_top7 %>%
         select(coverage, starts_with('usd_sum_') & !usd_sum_glucagon) %>%
         filter(coverage != 'pnta') %>%
         mutate('total_sum'=category_sum(df=., cols=c('usd_sum_devices', 'usd_sum_insulins', 'usd_sum_pen_needles', 'usd_sum_strips')))

df_3e %>%
  select(coverage, total_sum) %>%
  group_by(coverage) %>%
  summarize('N'=n(),
            'NAs'=paste0(as.character(sum(is.na(total_sum))), ' (', as.character(round(sum(is.na(total_sum))/n()*100, 1)), '%)'),
            'mean'=mean(total_sum, na.rm=T) %>% round(1),
            'median'=median(total_sum, na.rm=T) %>% round(1),
            'sd'=sd(total_sum, na.rm=T) %>% round(1)) %>%
  gridExtra::grid.table(rows=NULL)

if(PLOT) {
  ggplot(df_3e, aes(x=total_sum+1, col=coverage)) +
         geom_density(lwd=1) +
         scale_x_log10(expand=c(0, 0.05, 0, 0)) +
         scale_color_brewer(palette='Set1') +
         cowplot::theme_cowplot() +
         theme(aspect.ratio=.3) +
         labs(title='per healthcare coverage', x='OoPEs (monthly USD+1, log-scale)')
  }

if(PLOT) {
  df_3e %>%
  select(-total_sum) %>%
  melt(id.vars='coverage', variable.name='category') %>%
  mutate('category'=str_remove(category, 'usd_sum_')) %>%
  ggplot(aes(x=value+1, col=coverage)) +
         geom_density(lwd=1) +
         facet_wrap(~category, scales='free_x', ncol=1) +
         scale_x_log10(expand=c(0, 0.05, 0, 0)) +
         scale_color_brewer(palette='Set1') +
         cowplot::theme_cowplot() +
         theme(aspect.ratio=1) +
         labs(title='per healthcare coverage', x='OoPEs (monthly USD+1, log-scale)')
  }

## f. per country ####
df_3f <- data_top7 %>%
         mutate('total_sum'=category_sum(df=., cols=c('usd_sum_devices', 'usd_sum_insulins', 'usd_sum_pen_needles', 'usd_sum_strips'))) %>%
         select(country_alpha2, total_sum)

df_3f %>%
  group_by(country_alpha2) %>%
  summarize('N'=n(),
              'NAs'=paste0(as.character(sum(is.na(total_sum))), ' (', as.character(round(sum(is.na(total_sum))/n()*100, 1)), '%)'),
              'mean'=mean(total_sum, na.rm=T) %>% round(1),
              'median'=median(total_sum, na.rm=T) %>% round(1),
              'sd'=sd(total_sum, na.rm=T) %>% round(1)) %>%
  arrange(desc(median)) %>%
  gridExtra::grid.table(rows=NULL)

if(PLOT) {
  ggplot(df_3f, aes(x=total_sum+1, col=country_alpha2)) +
         geom_density(lwd=1) +
         scale_x_log10(expand=c(0, 0.05, 0, 0)) +
         scale_color_brewer(palette='Paired', name='', direction=-1) +
         cowplot::theme_cowplot() + theme(aspect.ratio=.4) +
         labs(title='per country', x='OoPEs (monthly USD+1, log-scale)')
  }

## g. breakdown ####
if(PLOT) {
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
  }


# 4. COVID-19 IMPACT -----------------------------------------------------------
## a. Impact overview (yes/no) ####
if(PLOT) {
  data_top7 %>%
    group_by(country_alpha2) %>%
    count(covid_impact_yesno) %>%
    group_by(country_alpha2) %>%
    mutate('total'=sum(n),
           'pc'=n/total*100,
           'covid_impact_yesno'=ifelse(covid_impact_yesno, 'Affected', 'Unaffected') %>% factor(levels=c('Unaffected', 'Affected')),
           'country_alpha2'=country_alpha2 %>% factor(levels=c('SE', 'GB', 'DE', 'CA', 'IN', 'US', 'PA'))) %>%
    ggplot(aes(x=country_alpha2, y=pc, fill=covid_impact_yesno)) +
           geom_col(lwd=.3, col='black', alpha=.55) +
           geom_hline(yintercept=50, lty=2) +
           geom_text(aes(label=n), position=position_stack(vjust=.5)) +
           cowplot::theme_cowplot() + theme(aspect.ratio=1.2) +
           scale_y_continuous(expand=c(0.01, 0)) +
           scale_fill_brewer(palette='Set1', name='', direction=-1) +
           labs(title='Overview of COVID-19 impact', x='Top 7 represented countries', y='% of responses')
  }

## b. Impact heatmap details (yes) ####
covid_yes_df <- data_top7 %>%
                filter(covid_impact_yesno) %>%
                select(country_alpha2, matches('covid_impact_') &! 'covid_impact_yesno') %>%
                melt(id.vars='country_alpha2', variable.name='category', value.name='impacted') %>%
                mutate('category'=category %>% str_remove('covid_impact_')) %>%
                filter(impacted) %>%
                count(country_alpha2, category) %>%
                group_by(country_alpha2) %>%
                mutate('total'=sum(n)) %>%
                ungroup() %>%
                mutate('pc'=n/total*100)

covid_mat_n <- covid_yes_df %>%
               select(-pc, -total) %>%
               pivot_wider(names_from='category', values_from='n') %>%
               replace_na(list('price_up'=0, 'pnta'=0, 'price_down'=0)) %>%
               column_to_rownames('country_alpha2')

if(PLOT) {
  pheatmap::pheatmap(covid_mat_n, cellwidth=30, cellheight=30, display_numbers=covid_mat_n,
                     cutree_rows=3, cutree_cols=3, main='COVID-19 impact details', scale='row')
  }



# 5. RATIONING -----------------------------------------------------------------
## a. worldwide rationing per healthcare coverage type ####
df_5a <- data %>%
         select(coverage, matches('ration_')) %>%
         melt(id.vars='coverage', variable.name='type', value.name='freq') %>%
         mutate('type'=case_when(type=='ration_insulin' ~ 'Ration or\nskip insulin',
                                 type=='ration_test' ~ 'Not testing\nblood glucose',)) %>%
         group_by_all() %>%
         summarize('n'=n()) %>%
         group_by(coverage) %>%
         mutate('total'=sum(n)) %>%
         ungroup() %>%
         filter(coverage != 'pnta') %>%
         filter(freq %!in% c(NA, 'pnta')) %>%
         mutate('freq'=factor(freq, levels=c('never', 'yearly', 'monthly', 'weekly', 'daily')))

if(PLOT) {
  ggplot(df_5a, aes(x=coverage, y=n, fill=freq)) +
         geom_col(lwd=.3, col='black', position=position_fill(), alpha=.75) +
         geom_text(aes(label=ifelse(n>5, n, '')), position=position_fill(vjust=.5)) +
         facet_grid(type~.) +
         coord_flip() + cowplot::theme_cowplot() + theme(aspect.ratio=.2, strip.background=element_blank()) +
         scale_y_continuous(expand=c(0, 0.01)) +
         scale_fill_brewer(palette='Spectral', name='Rationing\nfrequency', direction=-1) +
         labs(x='Healthcare coverage type', y='% of responses',
              title='Worldwide rationing frequencies by healthcare coverage type',
              subtitle='Not considering "NA" or "Prefer not to answer"')
  }

## b. worldwide rationing per country income level ####
df_5b <- data %>%
         select(country_income_class, matches('ration_')) %>%
         melt(id.vars='country_income_class', variable.name='type', value.name='freq') %>%
         mutate('type'=case_when(type=='ration_insulin' ~ 'Ration or\nskip insulin',
                                 type=='ration_test' ~ 'Not testing\nblood glucose',)) %>%
         group_by_all() %>%
         summarize('n'=n()) %>%
         group_by(country_income_class) %>%
         mutate('total'=sum(n)) %>%
         ungroup() %>%
         filter(freq %!in% c(NA, 'pnta')) %>%
         mutate('freq'=factor(freq, levels=c('never', 'yearly', 'monthly', 'weekly', 'daily')))

if(PLOT) {
    ggplot(df_5b, aes(x=country_income_class, y=n, fill=freq)) +
           geom_col(lwd=.3, col='black', position=position_fill(), alpha=.75) +
           geom_text(aes(label=n), position=position_fill(vjust=.5)) +
           facet_grid(type~.) +
           coord_flip() + cowplot::theme_cowplot() + theme(aspect.ratio=.2, strip.background=element_blank()) +
           scale_y_continuous(expand=c(0, 0.01)) +
           scale_fill_brewer(palette='Spectral', name='Rationing\nfrequency', direction=-1) +
           labs(x='Country income level', y='% of responses',
                title='Worldwide rationing frequencies per country income level',
                subtitle='Not considering "NA" or "Prefer not to answer"')
  }

## c. Rationing in top 7 represented countries ####
df_5c <- data_top7 %>%
         select(id, country_alpha2, matches('ration_')) %>%
         melt(id.vars=c('id', 'country_alpha2'), variable.name='ration_type', value.name='freq') %>%
         mutate('ration_type'=str_remove(ration_type, 'ration_') %>% factor(levels=c('insulin', 'test'))) %>%
         filter(freq %in% c('never', 'yearly', 'monthly', 'weekly', 'daily')) %>%
         mutate('freq'=factor(freq, levels=c('never', 'yearly', 'monthly', 'weekly', 'daily')),
                'ration_yesno'=ifelse(freq=='never', 'No', 'Yes')) %>%
         select(-id) %>%
         group_by_all() %>%
         summarize('n'=n()) %>%
         group_by(country_alpha2, ration_type) %>%
         mutate('total'=sum(n),) %>%
         ungroup() %>%
         mutate('pc'=n/total*100,
                'country_alpha2'=factor(country_alpha2, levels=c('SE', 'GB', 'DE', 'PA', 'CA', 'US', 'IN')))

df_5c %>%
  select(-country_alpha2, -total, -pc) %>%
  group_by(ration_type, freq, ration_yesno) %>%
  summarize('total_n'=sum(n)) %>%
  ggplot(aes(x=ration_yesno, y=total_n, fill=freq)) +
         geom_col(linewidth=.3, col='black', width=.8) +
         geom_text(aes(label=total_n), position=position_stack(vjust=.5)) +
         facet_wrap(~ration_type) +
         cowplot::theme_cowplot() + theme(aspect.ratio=2.5) +
         scale_y_continuous(expand=c(0, 0, 0.01, 0)) +
         scale_fill_brewer(palette='Spectral', name='Rationing\nfrequency', direction=-1) +
         labs(title='Rationing in top 7 countries', x='Rationing?', y='Number of respondents')

if(PLOT) {
    ggplot(df_5c, aes(x=interaction(ration_type, ration_yesno, lex.order=T), y=pc, fill=freq)) +
           geom_hline(yintercept=c(25, 50, 75), lty=2, col='grey80') +
           geom_col(linewidth=.3, col='black', alpha=.75) +
           geom_text(aes(label=ifelse(pc > 4, n, '')), position=position_stack(vjust=.5)) +
           cowplot::theme_cowplot() + theme(aspect.ratio=4, axis.text.x=element_text(angle=90)) +
           scale_y_continuous(expand=c(0, 0)) +
           facet_grid(~country_alpha2) +
           scale_fill_brewer(palette='Spectral', name='Rationing\nfrequency', direction=-1) +
           labs(title='Rationing in top 7 most represented countries', x='', y='% of responses',
                subtitle='Not considering "NA" or "Prefer not to answer"')
  }



# 6. DEMOGRAPHICS --------------------------------------------------------------
## a. worldwide ####
data %>%
  select(gender, T1con, usd_household_month, country_income_class) %>%
  mutate('gender'=factor(gender, levels=c('female', 'male', 'other', 'pnta')),
         'T1con'=factor(T1con, levels=c('patient', 'mychild', 'betterhalf', 'doc', 'pnta')),
         'country_income_class'=factor(country_income_class, levels=c('Low', 'Middle', 'High')),
         'usd_household_month'=cut(usd_household_month, breaks=c(0, 1000, 1500, 3000, 5000, Inf), include.lowest=T, right=F, ordered_result=T, dig.lab=5)) %>%
  unlist() %>%
  table() %>%
  enframe(name='response', value='n') %>%
  mutate('prop'=paste0('(', round(n/nrow(data)*100, 1), '%)'),
         'characteristic'=c(rep('gender', 4), rep('connection to T1D', 4), rep('monthly household income (USD)', 5), rep('Country income level', 3))) %>%
  select(characteristic, response, n, prop) %>%
  gridExtra::grid.table(rows=NULL)

## b. top 7 ####
data_top7 %>%
  select(gender, T1con, usd_household_month, country_income_class) %>%
  mutate('gender'=factor(gender, levels=c('female', 'male', 'other', 'pnta')),
         'T1con'=factor(T1con, levels=c('patient', 'mychild', 'betterhalf', 'doc', 'pnta')),
         'country_income_class'=factor(country_income_class, levels=c('Low', 'Middle', 'High')),
         'usd_household_month'=cut(usd_household_month, breaks=c(0, 1000, 1500, 3000, 5000, Inf), include.lowest=T, right=F, ordered_result=T, dig.lab=5)) %>%
  unlist() %>%
  table() %>%
  enframe(name='response', value='n') %>%
  mutate('prop'=paste0('(', round(n/nrow(data)*100, 1), '%)'),
         'characteristic'=c(rep('gender', 4), rep('connection to T1D', 4), rep('monthly household income (USD)', 5), rep('Country income level', 3))) %>%
  select(characteristic, response, n, prop) %>%
  gridExtra::grid.table(rows=NULL)


# 7. EXTRA ANALYSES --------------------------------------------------------------
## % CGM & pump users per country
data_top7 %>%
  select('country'=country_alpha2, insulin_intake_pump, cgm_yesno) %>%
  mutate('cgm_yesno'=ifelse(cgm_yesno=='yes', TRUE, FALSE)) %>%
  group_by(country) %>%
  summarize('n_respondents'=n(),
            'n_pump'=sum(insulin_intake_pump, na.rm=T),
            'n_cgm'=sum(cgm_yesno, na.rm=T),
            'pc_pump'=round(mean(insulin_intake_pump, na.rm=T)*100, 2),
            'pc_cgm'=round(mean(cgm_yesno, na.rm=T)*100, 2)) %>%
  arrange(desc(pc_cgm), desc(pc_pump)) %>%
  gridExtra::grid.table(rows=NULL)


