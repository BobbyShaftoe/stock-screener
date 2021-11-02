library(dplyr)
library(quantmod)
library(tidytable)
library(tidyr)
library(tidyverse)
library(XML)
library(rvest)
library(janitor)


#mining_stocks <- read_html("https://www.listcorp.com/asx/sectors/materials/materials/metals-mining/")
mining_stocks <- read_csv("MaterialsCompaniesTidy.csv", col_names = T)


calculate_volume <- function(ticker){
   z <- df_all_stocks_prices %>%  filter(ax_ticker==ticker) %>%  
      filter(Date>"2021-02-01") %>% mutate(V=as.numeric(VOLUME)) %>% select(V)
   return(mean(z$V) %>% as.integer())
}

compress_factors <- function(x, base, compression) {
   z <- x^2 %>% log(base = base*1.1) %>% round(digits = -compression/10) 
   return(factor(z, levels = unique(sort(z)), ordered = T))
}

reduce_if_negative <- function(df, data_col, ref_col) {
   for (row in 1:nrow(df)) {
      if (df[row, ref_col] <0){
         df[row, data_col] <- (df[row, data_col] / 100) * -1
      }
   }
   return(df)
}

create_ridgydidge_metric <- function(stock_df){
   enterprise_value_ebitda <- stock_df$enterprise_value_ebitda %>% as.double()
   enterprise_value_ebitda[is.na(enterprise_value_ebitda)] = 0.0
   
   pe_ratio_ttm <- stock_df$pe_ratio_ttm %>% as.double()
   pe_ratio_ttm[is.na(pe_ratio_ttm)] = 0.0
   
   profit_margin_percent <- stock_df$profit_margin_percent %>% as.double()
   profit_margin_percent[is.na(profit_margin_percent)] = 0.0
   
   ridgydidge <- 1 / (enterprise_value_ebitda + 1) %>% round(digits = 2)
   print(ridgydidge)
   
}




#https://sg.finance.yahoo.com/quote/FMG.AX/key-statistics?p=FMG.AX
get_yahoo_statistics <- function(ticker){
   stock_statistics <- data_frame()
   remove_col_names = c("Previous close", "Open", "Bid", "Ask", "Volume", "Day's range")
   yahoo_url <- paste0("https://sg.finance.yahoo.com/quote/", ticker, "/key-statistics?p=", ticker)
   print(yahoo_url)
   
   tryCatch({
      stock_summary <- read_html(yahoo_url)
      
      t_col_1 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>% 
         .[[1]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%  
         as_tibble()
      
      t_col_2 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>% 
         .[[2]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>% 
         as_tibble() 
      
      t_col_3 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>%
         .[[3]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%
         as_tibble()

      t_col_4 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>%
         .[[4]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%
         as_tibble()

      t_col_6 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>%
         .[[6]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%
         as_tibble()

      t_col_7 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>%
         .[[7]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%
         as_tibble()

      t_col_8 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>%
         .[[8]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%
         as_tibble()

      t_col_9 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>%
         .[[9]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%
         as_tibble()

      t_col_10 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>%
         .[[10]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%
         as_tibble()
      
#      stock_statistics <- cbind(t_col_1, t_col_2) %>% select(-remove_col_names) %>% clean_names()
      stock_statistics <- cbind(t_col_1, t_col_2, t_col_3, t_col_4, t_col_6, t_col_7, t_col_8, t_col_9, t_col_10)  %>% clean_names()
      stock_statistics$Code <- ticker
      print(colnames(stock_statistics))
   },
   error=function(e){}
   )
   return(stock_statistics)
}




get_yahoo_financials <- function(ticker){
   stock_financials <- data_frame()
   remove_col_names = c("Previous close", "Open", "Bid", "Ask", "Volume", "Day's range")
   yahoo_url <- paste0("https://sg.finance.yahoo.com/quote/", ticker, "?p=", ticker)
   print(yahoo_url)
   
   tryCatch({
   stock_summary <- read_html(yahoo_url)
   
      t_col_1 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>% 
         .[[1]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>%  
         as_tibble()
      
      t_col_2 <- stock_summary %>% html_nodes("table") %>% html_table(trim = T, dec=".") %>% 
         .[[2]] %>%  as_tibble(.name_repair = "unique") %>%  t() %>% janitor::row_to_names(1) %>% 
         as_tibble() 
      
      stock_financials <- cbind(t_col_1, t_col_2) %>% select(-remove_col_names) %>% clean_names()
      stock_financials$Code <- ticker
   },
     error=function(e){}
   )
   return(stock_financials)
}



get_reuters_about_stock <- function(ticker){
   reuters_url <- paste0("https://www.reuters.com/companies/", ticker, "/profile")
   print(reuters_url)
   
   tryCatch({
      stock_summary <- read_html(reuters_url)
      
      details_text <- stock_summary %>% html_nodes(css = "div.Profile-about-1d-H- p") %>% html_text()
      stock_details <- tibble(text = details_text, Code = ticker)
      print(stock_details)
   },
     error=function(e){}
   )
   return(stock_details)
}





# convert_any_to_numeric - helper function
convert_any_to_numeric <- function(value){
   value_numeric <- value %>% as.character() %>%  
      gsub("(\\.?0*%)|,", "", .) %>% gsub("N/A", "0", .) %>% as.double() %>% round(digits = 2)
   return(value_numeric)
}

# cleanup_data_values
cleanup_data_values <- function(data_frame_tmp, selected_columns, convert_symbols) {
   data_frame_tmp[selected_columns] <- na_if(data_frame_tmp[selected_columns], "(N/A|NA|NaN)")
   character_columns <- data_frame_tmp %>% select(c(selected_columns)) %>% colnames()
   
   for (chr_column in character_columns) {
      data_frame_tmp[chr_column] <- data_frame_tmp[[chr_column]] %>%  as.character()
      data_frame_tmp[chr_column][data_frame_tmp[chr_column] == "Inf"] = NA
      data_frame_tmp[chr_column][data_frame_tmp[chr_column] == "NaN"] = NA
      
      data_frame_tmp[chr_column][is.na(data_frame_tmp[chr_column])] = "0.0"
      
      if (convert_symbols == TRUE){
         data_frame_tmp[chr_column] <- data_frame_tmp[[chr_column]] %>% convert_any_to_numeric
      } else {
         data_frame_tmp[chr_column] <- data_frame_tmp[[chr_column]] %>% as.double() %>% round(digits = 2)
      }
   }
   return(data_frame_tmp)
}

# convert_percent_columns - wrapper around cleanup_data_values
convert_percent_columns <- function(data_frame_tmp, selected_columns, convert_symbols){
   data_frame_tmp <- cleanup_data_values(data_frame_tmp, selected_columns, convert_symbols = TRUE)
   for (chr_column in selected_columns) {
      data_frame_tmp[paste0(chr_column, "_percent")] <- data_frame_tmp[chr_column]
      data_frame_tmp <- data_frame_tmp %>% select(-c(chr_column))
   }
   return(data_frame_tmp)
}


# convert_unit_to_numeric
convert_unit_to_numeric <- function(target_df, column_name){
   target_df$tmp <- target_df[[column_name]]
   
   mutated_column <- target_df %>% mutate(`tmp`, 
       multiplier=ifelse(str_detect(`tmp`,"(M|m)"),1, ifelse(str_detect(`tmp`,"(K|k)"),0.001, ifelse(str_detect(`tmp`,"(B|b)"),1000,1))),
        `tmp`=str_remove(`tmp`,"(K|k|M|m|B|b)"),    
        `tmp`=as.double(`tmp`)*multiplier) %>% select(`tmp`)
   
   target_df[paste0(column_name, "_millions")] <- mutated_column$`tmp` %>% round(digits = 2) 
   target_df <- target_df %>%  select(-c("tmp", column_name))
   return(target_df)
}



# ------------------------------------   mining_stocks_prices ------------------------------------ 
df_all_stocks_prices <- read_csv("all_valid_stocks.csv")
df_all_stocks_latest <- df_all_stocks_prices %>% filter(Date=="2021-02-12")

mining_stocks$ax_ticker <- mining_stocks$Code

mining_stocks_prices <- data_frame()
mining_stocks_prices <- df_all_stocks_latest %>% inner_join(mining_stocks, by = "ax_ticker") %>% 
   select(c(-ax_ticker, -Change, -OPEN, -HIGH, -LOW, -ADJUSTED)) %>% na.omit()
 
write.csv(file = "mining_stocks_prices.csv", mining_stocks_prices)

mining_stocks_prices$Volume_Average2W <- sapply(mining_stocks_prices$Code, calculate_volume) 

mining_stocks_prices <- mining_stocks_prices %>%  clean_names() %>% 
   rename(Code = code) %>% select(c("company", "date", "close", "Code"))



# ------------------------------------   mining_stocks_statistics ------------------------------------ 
# Get financials for stocks and save a copy
#mining_stocks_statistics <- map_dfr(mining_stocks_prices$Code, get_yahoo_statistics)
#write.csv(file = "mining_stocks_statistics.csv", mining_stocks_statistics)
#Use cached and remove a bunch of unwanted columns
mining_stocks_statistics_cached <- read_csv("mining_stocks_statistics.csv", skip_empty_rows = T) %>% 
    select(-c(contains("short"), 
              "forward_p_e_1", "peg_ratio_5_yr_expected_1", "x5_year_average_dividend_yield_4", 
              "dividend_date_3", "forward_annual_dividend_rate_4", "trailing_annual_dividend_rate_3",
              "net_income_avi_to_common_ttm", "quarterly_earnings_growth_yoy", "last_split_factor_2",
              "diluted_eps_ttm", "price_sales_ttm", "avg_vol_10_day_3", "ebitda", "revenue_per_share_ttm",
              "s_p500_52_week_change_3", "enterprise_value_revenue_3",
              "operating_cash_flow_ttm", "total_debt_mrq", "current_ratio_mrq", "market_cap_intra_day_5",
              "last_split_date_3", "avg_vol_3_month_3", "trailing_p_e",
              "percent_held_by_institutions_1", "total_cash_mrq", "return_on_assets_ttm", "payout_ratio_4",
              "beta_5y_monthly", "revenue_ttm", "x50_day_moving_average_3", "forward_annual_dividend_yield_4", 
              "gross_profit_ttm", "ex_dividend_date_4"))
# Clean names some more
colnames(mining_stocks_statistics_cached) <- mining_stocks_statistics_cached %>% names %>% gsub("_[0-9]$", "", .) 
mining_stocks_statistics_cached <- convert_unit_to_numeric(mining_stocks_statistics_cached, "enterprise_value")
mining_stocks_statistics_cached <- convert_unit_to_numeric(mining_stocks_statistics_cached, "float")
mining_stocks_statistics_cached <- convert_unit_to_numeric(mining_stocks_statistics_cached, "levered_free_cash_flow_ttm")
mining_stocks_statistics_cached <- convert_unit_to_numeric(mining_stocks_statistics_cached, "shares_outstanding") 

mining_stocks_statistics_cached <- cleanup_data_values(mining_stocks_statistics_cached, 
        c("price_book_mrq","enterprise_value_ebitda", "x52_week_high", "x52_week_low", "x200_day_moving_average", 
          "float_millions", "total_cash_per_share_mrq", "total_debt_equity_mrq", "book_value_per_share_mrq", 
          "enterprise_value_millions", "levered_free_cash_flow_ttm_millions"), FALSE)

mining_stocks_statistics_cached <- convert_percent_columns(mining_stocks_statistics_cached, 
         c("x52_week_change", "percent_held_by_insiders", 
            "trailing_annual_dividend_yield", "profit_margin", "operating_margin_ttm", 
            "quarterly_revenue_growth_yoy", "return_on_equity_ttm"), FALSE)




# ------------------------------------   mining_stocks_financials ------------------------------------ 
# Get financials for stocks and save a copy
#mining_stocks_financials <- map_dfr(mining_stocks_prices$Code, get_yahoo_financials)
#write.csv(file = "mining_stocks_financials.csv", mining_stocks_financials)
#Use cached
mining_stocks_financials_cached <- read_csv("mining_stocks_financials.csv")
mining_stocks_financials_cached <- mining_stocks_financials_cached %>% select(c("market_cap", "pe_ratio_ttm", "avg_volume", "Code"))
mining_stocks_financials_cached <- convert_unit_to_numeric(mining_stocks_financials_cached, "market_cap")
mining_stocks_financials_cached <- cleanup_data_values(mining_stocks_financials_cached, c("avg_volume","pe_ratio_ttm"), TRUE)




# ------------------------------------   mining_stocks_details ------------------------------------ 
# Get details for stocks and save a copy
# mining_stocks_details <- map_dfr(mining_stocks_prices$Code, get_reuters_about_stock)
# write.csv(file = "mining_stocks_details.csv", mining_stocks_details)
#Use cached
mining_stocks_details_cached <- read_csv("mining_stocks_details.csv")
mining_stocks_details_cached <- mining_stocks_details_cached %>% rename(details = text)


# ------------------------------------   Merge dataframes ------------------------------------ 

mining_stocks_prices_merged <- mining_stocks_prices %>% left_join(mining_stocks_financials_cached, by = "Code") 
mining_stocks_prices_merged <- mining_stocks_prices_merged %>% left_join(mining_stocks_statistics_cached, by = "Code") 
mining_stocks_prices_merged <- mining_stocks_prices_merged %>% left_join(mining_stocks_details_cached, by = "Code") 

mining_stocks_prices_tidy <- mining_stocks_prices_merged %>%
   relocate(c("Code", "company", "details", "date", "close", "x52_week_high", "x52_week_low", "x200_day_moving_average", "x52_week_change_percent", "avg_volume")) %>% 
      rename(x200_ma = x200_day_moving_average) %>% 
      rename(x52_high = x52_week_high) %>% 
      rename(x52_low = x52_week_low) %>% 
      rename(x52_chg_pct = x52_week_change_percent) %>%    
      rename(avg_vol = avg_volume) %>% 
          select(-starts_with("X1"))

# Final NA to numeric 0  cleanup
numeric_columns <- mining_stocks_prices_tidy %>% select_if(., is.numeric) %>% colnames()
mining_stocks_prices_tidy <- cleanup_data_values(mining_stocks_prices_tidy, numeric_columns, FALSE)

write.csv(file = "mining_stocks_prices_tidy.csv", mining_stocks_prices_tidy)


# --------------- Final column selection ---------------
selected_columns <- c("Code", "company", "details", "date", "close", "x52_high", "x52_low", "x200_ma",
                      "x52_chg_pct", "avg_vol", "pe_ratio_ttm", "price_book_mrq", "market_cap_millions", 
                      "shares_outstanding_millions", "return_on_equity_ttm_percent", "total_debt_equity_mrq",
                      "profit_margin_percent", "operating_margin_ttm_percent", "enterprise_value_ebitda", 
                      "quarterly_revenue_growth_yoy_percent")

mining_stocks_prices_final <- mining_stocks_prices_tidy %>% select(selected_columns)


# -------------- Create new metrics -------------- 

# Price Volatility
mining_stocks_prices_final <- mining_stocks_prices_final %>% 
   mutate(price_vol = round(1 / (mining_stocks_prices_final$x200_ma / (mining_stocks_prices_final$x52_high - mining_stocks_prices_final$x52_low)), digits = 2)) %>% 
   relocate(price_vol, .before=avg_vol)
# Price Diff
# mining_stocks_prices_final <- mining_stocks_prices_final %>% 
#    mutate(price_diff = round(log(mining_stocks_prices_final$x52_high / mining_stocks_prices_final$x52_low), digits = 2)) %>% 
#    relocate(price_diff, .before=price_vol)
# # Enterprise Value Ebitda / Market Cap
# mining_stocks_prices_final <- mining_stocks_prices_final %>% 
#    mutate(eva_mc = round(mining_stocks_prices_final$market_cap_millions / (mining_stocks_prices_final$market_cap_millions - mining_stocks_prices_final$market_cap_millions / mining_stocks_prices_final$enterprise_value_ebitda) / mining_stocks_prices_final$operating_margin_ttm_percent, digits = 4) * 100) %>% 
#    relocate(eva_mc, .before=market_cap_millions)

# Shares Market Cap
mining_stocks_prices_final <- mining_stocks_prices_final %>% 
   mutate(shares_market_cap = round(market_cap_millions / shares_outstanding_millions, digits = 2), .before=shares_outstanding_millions) %>% 
   relocate(price_vol, .before=avg_vol)

mining_stocks_prices_final <- cleanup_data_values(mining_stocks_prices_final, "shares_market_cap", FALSE)

# mining_stocks_prices_final <- cleanup_data_values(mining_stocks_prices_final, c("price_vol", "price_diff", "eva_mc"), convert_symbols = FALSE)


#  --------------- Generate Z Scores ---------------
# PE Ratio
mining_stocks_prices_final <- mining_stocks_prices_final %>% 
   mutate(pe_z = round((pe_ratio_ttm - mean(pe_ratio_ttm))/sd(pe_ratio_ttm), digits = 2)) %>% 
   relocate(pe_z, .before=pe_ratio_ttm)
# Price to Book Ratio
mining_stocks_prices_final <- mining_stocks_prices_final %>% 
   mutate(pbr_z = round((price_book_mrq - mean(price_book_mrq))/sd(price_book_mrq), digits = 2)) %>% 
   relocate(pbr_z, .before=price_book_mrq)
# Debt to Equity Ratio
mining_stocks_prices_final <- mining_stocks_prices_final %>% 
   mutate(der_z = round((total_debt_equity_mrq - mean(total_debt_equity_mrq))/sd(total_debt_equity_mrq), digits = 2)) %>% 
   relocate(der_z, .before=total_debt_equity_mrq)

# Shares / Market Cap
mining_stocks_prices_final <- mining_stocks_prices_final %>% 
   mutate(smc_z = round((shares_market_cap - mean(shares_market_cap))/sd(shares_market_cap), digits = 2)) %>% 
   relocate(smc_z, .before=shares_market_cap)

# Price Volatility
# mining_stocks_prices_final <- mining_stocks_prices_final %>% 
#    mutate(pv_z = round((price_vol - mean(price_vol))/sd(price_vol), digits = 2)) %>% 
#    relocate(pv_z, .before=price_vol)
# # Price Diff
# mining_stocks_prices_final <- mining_stocks_prices_final %>% 
#    mutate(pd_z = round((price_diff - mean(price_diff))/sd(price_diff), digits = 2)) %>% 
#    relocate(pd_z, .before=price_diff)
# # Enterprise Value Ebitda / Market Cap
# mining_stocks_prices_final <- mining_stocks_prices_final %>% 
#    mutate(eva_mc_z = round((eva_mc - mean(eva_mc))/sd(eva_mc), digits = 2)) %>% 
#    relocate(eva_mc_z, .before=eva_mc)


# Adjust quarterly growth yoy against margin
x <- reduce_if_negative(mining_stocks_prices_final, "quarterly_revenue_growth_yoy_percent", ref_col = "profit_margin_percent")
mining_stocks_prices_final$qgyoy_adjusted <- x$quarterly_revenue_growth_yoy_percent
mining_stocks_prices_final <- mining_stocks_prices_final %>% relocate(qgyoy_adjusted, .before=quarterly_revenue_growth_yoy_percent)



# Factorize and generate Z Score for yoy revenue
y <- compress_factors(mining_stocks_prices_final$qgyoy_adjusted, 1, 5) %>% 
   as.integer()
mining_stocks_prices_final$qrgyoy <- y
mining_stocks_prices_final <- mining_stocks_prices_final %>% relocate(qrgyoy, .before=quarterly_revenue_growth_yoy_percent)

mining_stocks_prices_final <- mining_stocks_prices_final %>% 
   mutate(qrgyoy_z = round((qrgyoy - mean(qrgyoy))/sd(qrgyoy), digits = 2)) %>% 
   relocate(qrgyoy_z, .before=qrgyoy)




# ------------------------------ Filter candidates ------------------------------
# x <- mining_stocks_prices_final %>%  filter(price_book_mrq<30) %>%  
#    filter(market_cap_millions>10) %>%  filter(operating_margin_ttm_percent>0) %>% filter(close>1)

# x <- mining_stocks_prices_final %>% filter(str_detect(details, "gold|Gold")) %>% filter(close>1) %>% 
#    filter(quarterly_revenue_growth_yoy_percent>=0)


x <- mining_stocks_prices_final  %>% filter(close>0.01) %>% 
   filter(quarterly_revenue_growth_yoy_percent>-10) %>% filter(shares_outstanding_millions>0)

# x <- x %>% mutate (shares_market_cap = round(market_cap_millions / shares_outstanding_millions, digits = 2), .before=shares_outstanding_millions) 
# 
# x <- x  %>% filter(shares_market_cap>1) %>% filter(pbr_z<0.3) %>% filter(qgyoy_adjusted>=0) %>% 
#    filter(operating_margin_ttm_percent>0) %>% 
#    filter(str_detect(details, "gold|Gold")) 
# 
# x <- x %>% filter(eva_mc<10) %>% filter(eva_mc>-2)


#Price Volatility
# x$price_volativity <- round(x$x200_ma /  (x$x52_high - x$x52_low), digits = 2)
# x$price_diff <- round(log(x$x52_high / x$x52_low), digits = 2)
#EVA/MC
#x$eva_mc <- round(x$market_cap_millions / (x$market_cap_millions - x$market_cap_millions / x$enterprise_value_ebitda) / x$operating_margin_ttm_percent, digits = 4) * 100



write.csv(file = "mining_stocks_prices_final_x.csv", x)



mining_stocks_prices_tidy_1c <- mining_stocks_prices_tidy %>% filter(`close`<=1) 
mining_stocks_prices_tidy_10d <- mining_stocks_prices_tidy %>% filter(`close`>10) 

mining_stocks_prices_tidy_1d <- mining_stocks_prices_tidy %>% filter(`close`>1) %>% filter(`close`<=10)
mining_stocks_prices_tidy_1d$market_cap_as_factor <- compress_factors(mining_stocks_prices_tidy_1d$market_cap_millions, 5, 5)

mining_stocks_prices_tidy_10d <- mining_stocks_prices_tidy %>% filter(`close`>1)
mining_stocks_prices_tidy_10d$market_cap_as_factor <- compress_factors(mining_stocks_prices_tidy_10d$market_cap_millions, 5, 5)
write.csv(file = "mining_stocks_prices_tidy_10d.csv", mining_stocks_prices_tidy_10d)




ggplot(mining_stocks_prices_tidy_10d, aes(Code, close)) + geom_point(aes(col=market_cap_as_factor, size=avg_vol)) + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))



