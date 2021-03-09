# Function to get moving averages 
get_mav_tb <- function(data = d, mav = 5) {
  data %>%
    mutate(across(c('vix', 'oil', 'fx_dees', 'ptax'), 
                  .fns = list(mav = ~as.numeric(data.table::frollmean(.x, mav))))) %>%
    select(date, contains(c('vix', 'oil', 'fx_dees', 'ptax', 'id'))) %>%
    drop_na()
}

# Function to find days before window
days_previous_window <- function(f = f, days = 2) {
  map_df(f$g, ~{
    tb = filter(f, g == .x)
    g = rep(.x, days)
    if (days == 1) {
      id = tb$id - 1
    } else if (days < 1) {
      stop("days must be at least equal to 1") }
    else {
      id = seq(tb$id - days, tb$id - 1)
    }
    
    final = tibble(g, id, days = days)
  })
}

# Function to find days after window
days_after_window <- function(l = l, days = 1) {
  map_df(l$g, ~{
    tb = filter(l, g == .x)
    g = rep(.x, days)
    if (days == 1) {
      id = tb$id + 1
    } else if (days < 1) {
      stop('days must be at least equal to 1')
    } else {
      id = seq(tb$id + 1, tb$id + days, 1)
    }
    
    final = tibble(g, id, days = days)
  })
}


# Function to find average value of variables
average_vars <- function(d = d, days_tibble) {
  d %>%
    drop_na %>%
    select(date, vix, oil, fx_dees, ptax, id) %>%
    full_join(days_tibble) %>%
    group_by(g) %>%
    summarize(days = mean(days),
              oil_mean = mean(oil), 
              vix_mean = mean(vix), 
              dees_mean = mean(fx_dees), 
              ptax_mean = mean(ptax)) %>%
    drop_na
}

# Function to extract r.squared from robustbase::lmrob
glance.lmrob <- function(x, ...) {
  s <- summary(x)
  
  tb <- tibble(
    r.squared = s$r.squared,
    adj.r.squared = s$adj.r.squared
  )
  return(tb)
}


