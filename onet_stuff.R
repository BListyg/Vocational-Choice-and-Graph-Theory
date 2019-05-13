library(rvest)
library(zoo)
library(tibble)

onet_data_url <- 
  read_html('https://www.onetcenter.org/db_releases.html') 
 
onet_data <- cbind(
  onet_data_url %>% 
    html_nodes('.badge-secondary') %>% 
    html_text() %>% 
    trimws() %>% 
    strsplit(x = ., split = ' ', fixed = T) %>% 
    do.call(rbind, .) %>% 
    data.frame() %>% 
    setNames(c('Month', 'Year'))
,
  onet_data_url %>% 
    html_nodes('.page-item:nth-child(1) .page-link') %>% 
    html_attr('href') %>% 
    paste("http://onetcenter.org",.) %>% 
    data.frame %>% 
    setNames('dataURL')
) %>% 
  tibble

bls_data <- 
  
cbind(
  
  c(rep(x = "May",14),
    "November",
    "May",
    "November",
    "May",
    NA,
    NA,
    NA,
    NA,
    NA,
    NA),
  
  read_html('https://stats.bls.gov/oes/tables.htm') %>% 
    html_nodes('h3') %>% 
    html_text() %>% 
    matrix %>% 
    trimws() %>% 
    .[c(1:(nrow(.)-2)),] %>% 
    matrix %>% 
    apply(X = ., MARGIN = 1, FUN = function(x) substr(x = x, start = nchar(x)-3, stop = nchar(x))) %>% 
    matrix
,
  c("https://www.bls.gov/oes/special.requests/oesm18nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm17nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm16nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm15nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm14nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm13nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm12nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm11nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm10nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm09nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm08nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm07nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm06nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm05nat.zip",
    "https://www.bls.gov/oes/special.requests/oesn04nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm04nat.zip",
    "https://www.bls.gov/oes/special.requests/oesn03nat.zip",
    "https://www.bls.gov/oes/special.requests/oesm03nat.zip",
    "https://www.bls.gov/oes/special.requests/oes02nat.zip",
    "https://www.bls.gov/oes/special.requests/oes01nat.zip",
    "https://www.bls.gov/oes/special.requests/oes00nat.zip",
    "https://www.bls.gov/oes/special.requests/oes99nat.zip",
    "https://www.bls.gov/oes/special.requests/oes98nat.zip",
    "https://www.bls.gov/oes/estimates_88_95.htm")
) %>% 
  data.frame %>% 
  setNames(c("Month","Year","dataURL")) %>% 
  tibble
  
