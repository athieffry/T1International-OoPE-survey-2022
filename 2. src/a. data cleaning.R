# T1International 2022 Survey: data cleaning ###
# Axel Thieffry - October 2022
library(tidyverse)
library(tidylog)
library(magrittr)
library(reshape2)
library(forcats)
library(janitor)
library(RColorBrewer)
library(ggrepel)
library(rworldmap)
library(WriteXLS)
library(lubridate)
library(priceR)
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
# extract currency symbol
extract_currency <- function(str) {
                                   if(is.na(str)) {
                                                  result <- NA
                                                  }
                                   else if(str_detect(str, '\\([:upper:]{3}\\)')) {
                                     result <- str_match(str, '\\([:upper:]{3}\\)') %>%
                                               str_remove('\\(') %>%
                                               str_remove('\\)')
                                     }
                                   else {
                                        result <- NA
                                        }
                                   return(result)
                                  }



# 1. READ ----------------------------------------------------------------------
# a. raw data
data <- readr::read_tsv('1. data/a. raw/Type1DiabetesOutofPo_DATA_LABELS_2022-10-13.tsv', col_names=T, trim_ws=T) # 1122 x 140
# b. new data column names
new_col_names <- readr::read_tsv('1. data/c. clean/simplified_column_names.tsv', col_names=T, trim_ws=T)
# c. country info
country_info <- readRDS('1. data/b. country_data/country_data.rds')



# 2. CLEAN ---------------------------------------------------------------------
# a. rename columns with simplified variable names
data %<>% set_colnames(new_col_names$renamed_column)

# b. remove empty rows and columns
data %<>% janitor::remove_empty(which=c('rows', 'cols'), quiet=F) # 1 empty column

# c. remove entries without explicit consent
data %<>% filter(consent=='I have read the above information and I agree to participate') %>% select(-consent) # 28 removed - new dim: 1094 x 139

# d. remove entries not major of age
data %<>% filter(major=='Yes') %>% select(-major) # 21 removed - new dim: 1094 x 139

# e. remove those without currency declared
to_keep <- data %>%
           mutate('currency'=ifelse(currency=='Your Currency', NA, currency)) %>%
           select(currency, currency_other_desc_manual) %>%
           is.na() %>%
           rowSums() %>%
           is_less_than(2)

data %<>% filter(to_keep) # 54 removed - new dim: 1019 x 138

# f. investigate duplicated records
any(duplicated(data)) # none

# g. remove timestamp
data %<>% select(-timestamp) # new dim: 1019 x 137

# h. add country info
    # remove entries without declared country
    data %<>% filter(!is.na(country_name)) # 5 removed - new dim: 1014 x 137
    # adapt country names to match with data
    country_info %<>% mutate(country_name = case_when(country_name == 'Bahamas (the)' ~ 'Bahamas, The',
                                                      country_name == 'Myanmar' ~ 'Burma',
                                                      TRUE ~ country_name))
    # standardize naming for South Korea in data
    data %<>% mutate('country_name'=ifelse(country_name=='Korea, South', 'South Korea', country_name))
    # join country info to data
    data %<>% left_join(country_info, by='country_name') # new dim: 1014 x 141



# 3. CURRENCIES ----------------------------------------------------------------
# a. currency exchange rates to USD
usd_rate_table <- exchange_rate_latest(currency='USD') %>% rename('currency_usd_factor'='one_usd_is_equivalent_to') %>% as_tibble()

# b. extract & standardize currency symbols from data
data %<>%
  # extract choice currency symbol
  mutate('currency_symbol'=sapply(data$currency, extract_currency, USE.NAMES=F)) %>%
  # if no choice, take currency symbol from manually-corrected "other"
  mutate('currency_symbol'=ifelse(is.na(currency_symbol), currency_other_desc_manual, currency_symbol)) %>%
  # add conversion rate to USD
  left_join(usd_rate_table, by=c('currency_symbol'='currency')) %>%
  # remove unnecessary columns
  select(-currency, -currency_other_desc, -currency_other_desc_manual) # 1014 records x 140 variables

# c. convert all costs to USD (in-situ)
  # divide amounts by USD factor
  data %<>% mutate(across(starts_with('cost'), \(x) divide_by(x, currency_usd_factor)))

  # remove unnecessary currency columns
  data %<>% select(-matches('currency')) # 1014 entries x 138 variables

  # rename columns from "cost_xxx" to "usd_xxx"
  colnames(data) <- colnames(data) %>% str_replace_all('cost_', 'usd_')

  # check that total corresponds to sum of individual OoPEs (Out-of-Pocket Expenses)
  if(PLOT) {
    cost_check_df <- tibble('id'=data$id,
                            'manual_sum'=data %>%
                                         select(usd_shortacting_month,
                                                usd_intlongacting_month,
                                                usd_mixed_month,
                                                usd_otheracting_month,
                                                usd_pumpsupplies_month,
                                                usd_strip_month,
                                                usd_cgm_month,
                                                usd_pen_syringes_month) %>%
                                         rowSums(na.rm=T),
                            'survey_sum'=data$usd_OoPE_total_month)

  # because of currency conversion float point operation, manual and survey sums won't be exactly identical
  # a solution could be to round the values to be compared, or to simply scatter-plot them and expect a close-to-perfect diagonal
    ggplot(cost_check_df, aes(x=manual_sum+1, y=survey_sum+1)) +
           geom_abline(intercept=0, slope=1, col='red') +
           geom_hline(yintercept=5000, lty=2, col='grey40') +
           geom_vline(xintercept=5000, lty=2, col='grey40') +
           annotate('text', x=2, y=6000, label='5000 USD') +
           geom_point() +
           geom_text(aes(label=ifelse(manual_sum>10000, paste('ID:', id), NA)), hjust=1.2) +
           theme(aspect.ratio=1) +
           scale_x_log10() + scale_y_log10() +
           labs(title='Costs sanity check', subtitle='Manual cost sum vs survey-calculated cost sum',
                x='log(manual_sum + 1) [USD]', y='log(survey_sum + 1) [USD]')
  }

  # SPECIAL CASES:
  # id 70 - South Korean participant declares USD currency. While possible, numbers are way too high (USD) or way too low (KRW). Removed.
  # id 725 - Mali participant declares EUR currency. While possible, numbers are way too high in EUR. XOF cannot be confirmed. Removed.
  # id 963 - costs do not make sense in any of the Venezuelan currencies, either way too cheap, either way too expensive. Removed.
  data %<>% filter(id %!in% c(70, 963, 725)) # new dim: 1011 x 138



# 4. RE-LEVEL & SANITY CHECKS  -------------------------------------------------
# a. connection to T1
data %<>% mutate('T1con'=case_when(T1con == 'I am a medical professional completing survey on behalf of a specific patient with type 1 diabetes' ~ 'doc',
                                   T1con == 'I have type 1 diabetes' ~ 'patient',
                                   T1con == 'My child has type 1 diabetes' ~ 'mychild',
                                   T1con == 'My spouse/partner/significant other has type 1 diabetes' ~ 'betterhalf',
                                   T1con == 'Prefer not to answer' ~ 'pnta'))

if(PLOT) {
  count(data, T1con) %>%
    mutate('T1con'=fct_reorder(T1con, n)) %>%
    ggplot(aes(x=T1con, y=n)) +
           geom_col(lwd=.2, col='black') +
           coord_flip() +
           geom_text(aes(label=n), hjust=-.3) +
           theme(aspect.ratio=.5) + scale_y_continuous(expand=c(0, 0, .1, 0)) +
           labs(title='Connection to T1D', x='', y=paste0('Nb. answers (N=', nrow(data),')'))
  }

# b. gender
# keep females, males, pnta, and coerce everything else as 'other'
data %<>% mutate('gender'=case_when(gender == 'Female' ~ 'female',
                                    gender == 'Male' ~ 'male',
                                    gender %in% c('Non-binary', 'Other {gender_oth}') ~ 'other',
                                    gender == 'Prefer not to answer' ~ 'pnta')) %>%
          select(-gender_other_desc, -gender_transgender)

if(PLOT) {
  count(data, gender) %>%
    mutate('gender'=fct_reorder(gender, n)) %>%
    ggplot(aes(x=gender, y=n)) +
           geom_col(lwd=.2, col='black') +
           coord_flip() +
           geom_text(aes(label=n), hjust=-.3) +
           theme(aspect.ratio=.5) + scale_y_continuous(expand=c(0, 0, .1, 0)) +
           labs(title='Simplified gender distribution', x='', y=paste0('Nb. answers (N=', nrow(data),')'))
  }

# c. country
n_distinct(data$country_alpha2, na.rm=T) # 69 different countries, 0 NA

country_summarized_df <- data %>%
                         select(starts_with('country_')) %>%
                         group_by_all() %>%
                         summarize('n'=n()) %>%
                         ungroup() %>%
                         mutate('country_alpha3'=forcats::fct_reorder(country_alpha3, n))

if(PLOT) {
  country_summarized_df %>%
    filter(n > 5) %>%
    ggplot(aes(x=country_alpha3, y=n, fill=country_income_class)) +
           geom_col(lwd=.3, col='black', width=.8) +
           geom_text(aes(label=n), hjust=-.2) +
           coord_flip() +
           scale_fill_brewer(palette='Set1', name='Income class') +
           scale_y_continuous(expand=c(.01, 0, .1, 0)) +
           theme(aspect.ratio=.5) +
           labs(title='Country distribution', subtitle='69 countries represented, 0 NA', x='Country alpha-3 code (ISO 3166)')


  worldmap <- joinCountryData2Map(country_summarized_df, joinCode='ISO3', nameJoinColumn='country_alpha3', verbose=F)
  mapCountryData(worldmap, nameColumnToPlot='n', missingCountryCol='grey70', colourPalette='heat', borderCol='black', catMethod='pretty', oceanCol='lightblue', numCats=451)
  }

# d. race
data %<>%
  mutate(across(starts_with('race_'), \(x) ifelse(x=='Checked', T, F))) %>%
  select(-race_other_desc)

if(PLOT) {
  data %>%
    select(starts_with('race_')) %>%
    multiply_by(1) %>%
    pheatmap::pheatmap(show_rownames=F, cellwidth=10, main='Race distribution',
                       legend_breaks=c(0, 1), color=c('navy', 'red'),
                       treeheight_row=20, treeheight_col=40, legend_labels=c('FALSE', 'TRUE'),
                       clustering_distance_rows='manhattan', clustering_method='ward.D2')

  data %>%
    select(starts_with('race_')) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('race_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Race')
  }

# e. health coverage
data %<>% mutate('coverage'=case_when(coverage=='No, there is no coverage for any of my costs' ~ 'none',
                                      coverage=='Yes, there is health coverage for <b>some</b> of my costs' ~ 'partial',
                                      coverage=='Yes, there is health coverage for <b>all</b> of my costs (so I do not pay anything out of pocket)' ~ 'full',
                                      coverage=='Prefer not to answer' ~ 'pnta') %>%
          factor(levels=c('pnta', 'none', 'partial', 'full')))

if(PLOT) {
  data %>%
    count(coverage) %>%
    mutate('coverage'=fct_reorder(coverage, n)) %>%
    ggplot(aes(x=coverage, y=n)) +
           geom_col(lwd=.3, col='black') +
           geom_text(aes(label=n), hjust=-.5) +
           coord_flip() +
           theme(aspect.ratio=.5, legend.position='none') +
           scale_y_continuous(expand=c(0, 0, .15, 0)) +
           labs(title='Healthcare coverage distribution', x='', y='Number of answers (N=1011)')
  }

# f. financial help
data %<>% mutate(across(starts_with('help_'), \(x) ifelse(x=='Checked', T, F)))

if(PLOT) {
  data %>%
    select(starts_with('help_') &! help_other_desc) %>%
    multiply_by(1) %>%
    pheatmap::pheatmap(show_rownames=F, cellwidth=10, main='Financial help',
                       legend_breaks=c(0, 1), color=c('navy', 'red'),
                       treeheight_row=20, treeheight_col=40, legend_labels=c('FALSE', 'TRUE'),
                       clustering_distance_rows='euclidean', clustering_method='ward.D2')

  data %>%
    select(matches('help_') &! help_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(str_remove(colnames(.), 'help_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Financial help')
  }

# g. things done to pay OoPE's
data %<>% mutate(across(starts_with('payOoPE_') & !payOoPE_other_desc, \(x) ifelse(x=='Checked', T, F)))

if(PLOT) {
  data %>%
    select(starts_with('payOoPE_') &! payOoPE_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('payOoPE_')) %>%
    pheatmap::pheatmap(show_rownames=F, color=c('navy', 'red'), cellwidth=15,
                       treeheight_row=20, treeheight_col=40,
                       breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('FALSE' ,'TRUE'),
                       clustering_method='ward.D2', main='Pay OoPE')

  data %>%
    select(matches('payOoPE'), -payOoPE_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('payOoPE_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Pay for OoPEs')
  }

# h. COVID-19 impact
data %<>% mutate(across(starts_with('covid_'), \(x) ifelse(x=='Checked', T, F)))

if(PLOT) {
  data %>%
    select(starts_with('covid_')) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('covid_')) %>%
    pheatmap::pheatmap(show_rownames=F, color=c('navy', 'red'), cellwidth=15,
                       treeheight_row=20, treeheight_col=40,
                       breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('FALSE' ,'TRUE'),
                       clustering_method='ward.D2', clustering_distance_rows='binary', main='COVID impact')

  data %>%
    select(starts_with('covid_')) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('covid_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='COVID impact')
  }

# i. insulin forms (vials, pens, ...)
data %<>% mutate(across(starts_with('insulin_form_') & !insulin_form_other_desc, \(x) ifelse(x=='Checked', T, F)))

if(PLOT) {
  data %>%
    select(starts_with('insulin_form_') &! insulin_form_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('insulin_form_')) %>%
    pheatmap::pheatmap(show_rownames=F, color=c('navy', 'red'), cellwidth=15,
                       treeheight_row=20, treeheight_col=40,
                       breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('FALSE' ,'TRUE'),
                       clustering_method='ward.D2', clustering_distance_rows='binary', main='Insulin forms (vials, pens, ...)')

  data %>%
    select(starts_with('insulin_form_') &! insulin_form_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('insulin_form_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Insulin forms (vials, pens, ...)')
}

# j. insulin intake methods (syringe, pump, ...)
data %<>% mutate(across(starts_with('insulin_intake_') & !insulin_intake_other_desc, \(x) ifelse(x=='Checked', T, F)))

if(PLOT) {
  data %>%
    select(starts_with('insulin_intake_') &! insulin_intake_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('insulin_intake_')) %>%
    pheatmap::pheatmap(show_rownames=F, color=c('navy', 'red'), cellwidth=15,
                       treeheight_row=20, treeheight_col=40,
                       breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('FALSE' ,'TRUE'),
                       clustering_method='ward.D2', clustering_distance_rows='binary', main='Insulin intake methods')

  data %>%
    select(starts_with('insulin_intake_') &! insulin_intake_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('insulin_intake_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Insulin intake methods')
}

# k. insulin brand
data %<>% mutate(across(starts_with('insulin_brand_') & !insulin_brand_other_desc, \(x) ifelse(x=='Checked', T, F)))

if(PLOT) {
  data %>%
    select(starts_with('insulin_brand_') &! insulin_brand_other_desc) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('insulin_brand_')) %>%
    pheatmap::pheatmap(show_rownames=F, color=c('navy', 'red'), cellwidth=10,
                       treeheight_row=20, treeheight_col=40,
                       breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('FALSE' ,'TRUE'),
                       clustering_method='ward.D2', clustering_distance_rows='euclidean', main='Insulin brands')

  top5_insulin_brands <- data %>%
                         select(starts_with('insulin_brand_') &! insulin_brand_other_desc) %>%
                         colSums() %>%
                         enframe() %>%
                         slice_max(order_by=value, n=5) %>%
                         pull(name)

  data %>%
    select(all_of(top5_insulin_brands)) %>%
    multiply_by(1) %>%
    set_colnames(colnames(.) %>% str_remove('insulin_brand_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Insulin brands (top 5)')
}

# l. consumable quantities: check for outliers
  # nothing to do here
  if(FALSE) { select(data, starts_with('shortacting_'), starts_with('intlongacting_'), starts_with('mixed_'), starts_with('otheracting_')) }

# m. frequency of rationing insulin intake
data %<>% mutate('ration_insulin'=case_when(ration_insulin == 'At least once per month' ~ 'monthly',
                                            ration_insulin == 'At least once per week' ~ 'weekly',
                                            ration_insulin == 'At least once per year' ~ 'yearly',
                                            ration_insulin == 'Every day' ~ 'daily',
                                            ration_insulin == 'Never' ~ 'never',
                                            ration_insulin == 'Prefer not to answer' ~ 'pnta',
                                            TRUE ~ ration_insulin) %>%
                   factor(levels=c('pnta', 'never', 'yearly', 'monthly', 'weekly', 'daily')))

if(PLOT) {
  ggplot(data, aes(x=ration_insulin)) +
         geom_bar(lwd=.3, col='black') +
         geom_text(stat='count', aes(label=..count..), hjust=-.3) +
         coord_flip() + scale_y_continuous(expand=c(0.01, 0, 0.1, 0)) +
         theme(aspect.ratio=.5) +
         labs(title='Rationing of insulin', x='Frequency', y='Nb. of respondents')
  }

# n. frequency of rationing BG testing
data %<>% mutate('ration_test'=case_when(ration_test == 'At least once per month' ~ 'monthly',
                                         ration_test == 'At least once per week' ~ 'weekly',
                                         ration_test == 'At least once per year' ~ 'yearly',
                                         ration_test == 'Every day' ~ 'daily',
                                         ration_test == 'Never' ~ 'never',
                                         ration_test == 'Prefer not to answer' ~ 'pnta',
                                         TRUE ~ ration_test) %>%
                   factor(levels=c('pnta', 'never', 'yearly', 'monthly', 'weekly', 'daily')))

if(PLOT) {
  ggplot(data, aes(x=ration_test)) +
         geom_bar(lwd=.3, col='black') +
         geom_text(stat='count', aes(label=..count..), hjust=-.3) +
         coord_flip() + scale_y_continuous(expand=c(0.01, 0, 0.1, 0)) +
         theme(aspect.ratio=.5) +
         labs(title='Rationing of BG testing', x='Frequency', y='Nb. of respondents')
  }

# o. pump, strip, and CGM info
data %<>% mutate('pump_type'=ifelse(pump_type=='Prefer not to answer', 'pnta', pump_type))

data %<>% mutate('strip_brand'=case_when(strip_brand=='Other {bg_strips_oth}' ~ 'Other',
                                         strip_brand=='Prefer not to answer' ~ 'pnta',
                                         TRUE ~ strip_brand))

data %<>% separate('cgm', c('cgm_yesno', 'cgm_brand'), sep=', ') %>%
          mutate('cgm_yesno'=ifelse(cgm_yesno=='Prefer not to answer', 'pnta', tolower(cgm_yesno))) %>%
          mutate('cgm_brand'=ifelse(cgm_brand=='other {cgm_oth}', 'other', cgm_brand))

if(PLOT) {
  # Top 5 pump types
  count(data, pump_type) %>%
    mutate('pump_type'=fct_reorder(pump_type, n)) %>%
    slice_head(n=5) %>%
    ggplot(aes(x=pump_type, y=n)) +
           geom_col(col='black', lwd=.3) + geom_text(aes(label=n), hjust=-.2) +
           coord_flip() + theme(aspect.ratio=.5) +
           scale_y_continuous(expand=c(0.01, 0, 0.1, 0)) + labs(title='Top 5 pump brands', x='', y='Nb. respondents')

  # Top 5 strip brands
  count(data, strip_brand) %>%
    mutate('strip_brand'=fct_reorder(strip_brand, n)) %>%
    slice_head(n=5) %>%
    ggplot(aes(x=strip_brand, y=n)) +
           geom_col(col='black', lwd=.3) + geom_text(aes(label=n), hjust=-.2) +
           coord_flip() + theme(aspect.ratio=.5) +
           scale_y_continuous(expand=c(0.01, 0, 0.1, 0)) + labs(title='Top 5 strip brands', x='', y='Nb. respondents')

  # Strips/month (numbers are way too high)
  ggplot(data, aes(y=strip_nb_month)) +
         geom_boxplot() +
         theme(aspect.ratio=1) +
         labs(title='Monthly number of test strips', y='Nb. strips (per month)')

  # Top 5 CGM
  data %>%
    mutate('cgm_yesno'=factor(cgm_yesno, levels=c('yes', 'no', 'pnta'))) %>%
    ggplot(aes(x=cgm_yesno, fill=cgm_brand)) +
           geom_bar(col='black', lwd=.3) +
           geom_text(stat='count', aes(label=..count..), position=position_stack(vjust=.5)) +
           scale_y_continuous(expand=c(0.01, 0)) + theme(aspect.ratio=1.5) +
           labs(title='CGM usage and brands', x='CGM?', y='Nb. of respondents')
  }

# p. glucagon
data %<>% separate('glucagon', c('glucagon_yesno', 'glucagon_type'), sep=', ') %>%
          mutate('glucagon_yesno'=case_when(glucagon_yesno=='Yes' ~ 'yes',
                                            glucagon_yesno=='No' ~ 'no',
                                            glucagon_yesno=='Prefer not to answer' ~ 'pnta',
                                            TRUE ~ glucagon_yesno) %>%
                                  factor(levels=c('yes', 'no', 'pnta')),
                 'glucagon_type'=case_when(glucagon_type=='Glucagon' ~ 'glucagon',
                                           glucagon_type=='nasal spray (Baqsimi)' ~ 'spray',
                                           TRUE ~ glucagon_type)) %>%
          mutate(across(starts_with('glucagon_no_') & !glucagon_no_other_desc, \(x) ifelse(x=='Checked', TRUE, FALSE)))

if(PLOT) {

  data %>%
    select(starts_with('glucagon_') & !glucagon_no_other_desc) %>%
    ggplot(aes(x=glucagon_yesno, fill=glucagon_yesno)) +
           geom_bar(lwd=.3, col='black') +
           geom_text(stat='count', aes(label=..count..), vjust=-.2) +
           theme(aspect.ratio=1.5, legend.position='none') +
           labs(title='Glucagon kit')

  data %>%
    select(starts_with('glucagon_no_') & !glucagon_no_other_desc) %>%
    set_colnames(colnames(.) %>% str_remove('glucagon_no_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Reasons for no glucagon kit')

}

# q. ketone kits
data %<>% separate('ketstrip', c('ketstrip_yesno', 'ketstrip_type'), sep=', ') %>%
          mutate('ketstrip_yesno'=case_when(ketstrip_yesno=='No' ~ 'no',
                                            ketstrip_yesno=='Yes' ~ 'yes',
                                            ketstrip_yesno=='Prefer not to answer' ~ 'pnta',
                                            TRUE ~ ketstrip_yesno) %>%
                                  factor(levels=c('yes', 'no', 'pnta')),
                 'ketstrip_type'=str_remove(ketstrip_type, 'urine ')) %>%
          mutate(across(starts_with('ketstrip_no') & !ketstrip_no_other_desc, \(x) ifelse(x=='Checked', TRUE, FALSE)))

if(PLOT) {

  data %>%
      select(starts_with('ketstrip_') & !ketstrip_no_other_desc) %>%
      ggplot(aes(x=ketstrip_yesno, fill=ketstrip_yesno)) +
             geom_bar(lwd=.3, col='black') +
             geom_text(stat='count', aes(label=..count..), vjust=-.2) +
             theme(aspect.ratio=1.5, legend.position='none') +
             labs(title='Ketone strips')

  data %>%
    select(starts_with('ketstrip_no_') & !ketstrip_no_other_desc) %>%
    set_colnames(colnames(.) %>% str_remove('ketstrip_no_')) %>%
    eulerr::euler(shape='ellipse') %>% plot(quantities=T, main='Reasons for no ketone test')

}

# r. plot all costs
if(PLOT) {

  data %>%
  select(id, starts_with('usd_')) %>%
  melt(id.vars='id', variable.name='cost', value.name='USD') %>%
  mutate('cost'=str_remove(cost, 'usd_')) %>%
  na.omit() %>%
  ggplot(aes(x=USD, col=cost)) +
         geom_density() +
         facet_wrap(~cost, scales='free') +
         scale_x_log10(labels=comma_format(accuracy=1)) +
         theme(legend.position='none') +
         labs(title='Distribution of costs',
              subtitle='Currency: USD',
              x='USD, log-scale',
              caption='0 USD are not shown')

  }



# 5. INCOMPLETE RECORDS --------------------------------------------------------
# a. number
data %>% count(complete)

# b. heatmap of missing answers for (tentative list of) important fields
if(PLOT) {
  data %>%
    filter(complete=='Incomplete') %>%
    select(id, T1con, country_alpha2, coverage, starts_with('ration_'), covid_unaffected,
           starts_with('usd_')) %>%
    column_to_rownames('id') %>%
    is.na() %>%
    multiply_by(1) %>%
    pheatmap::pheatmap(cellwidth=15, cellheight=15, cutree_rows=2, main='Incomplete records',
                       show_rownames=T, cluster_cols=F, clustering_method='ward.D2',
                       legend_breaks=c(0, 1), color=c('lightblue', 'red'), gaps_col=c(3, 5, 6, 18))

  }



# 6. EXPORT CLEAN DATA --------------------------------------------------------
# a. RDS format
saveRDS(data, '1. data/c. clean/cleaned_data.rds')

# b. TSV format
write_tsv(data, '1. data/c. clean/cleaned_data.tsv', col_names=T)
