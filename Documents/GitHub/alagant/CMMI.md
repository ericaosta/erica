# CMMI

How to scrap data from [CMMI Institute](https://cmmiinstitute.com) and clean to select only >= ML3 with SAM

### Load packages
```r
library(rvest)
library(dplyr)
library(pander)
library(stringr)
library(xml2)
library(tibble)
library(tidyverse)
```


### Scrap whole item-card data from all pages; separate into 500 pages at the time

```
itemcard_1_500 <- lapply(paste0('https://cmmiinstitute.com/pars/?StateId=33296d0d-c5c5-4d47-b36b-c692d73a5ab7&PageNumber=', 1:500),
                function(url){
                  url %>% read_html() %>% 
                    html_nodes("div.item-card") %>% 
                    html_text()
                })

itemcard_501_1000 <- lapply(paste0('https://cmmiinstitute.com/pars/?StateId=33296d0d-c5c5-4d47-b36b-c692d73a5ab7&PageNumber=', 501:1000),
                function(url){
                  url %>% read_html() %>% 
                    html_nodes("div.item-card") %>% 
                    html_text()
                })

itemcard_1001_1500 <- lapply(paste0('https://cmmiinstitute.com/pars/?StateId=33296d0d-c5c5-4d47-b36b-c692d73a5ab7&PageNumber=', 1001:1500),
                function(url){
                  url %>% read_html() %>% 
                    html_nodes("div.item-card") %>% 
                    html_text()
                })

itemcard_1500_1913 <- lapply(paste0('https://cmmiinstitute.com/pars/?StateId=33296d0d-c5c5-4d47-b36b-c692d73a5ab7&PageNumber=', 1501:1913),
                function(url){
                  url %>% read_html() %>% 
                    html_nodes("div.item-card") %>% 
                    html_text()
                })
```

### Turn lists into dataframes 
```
df_1_500 <- as.data.frame(do.call(rbind, lapply(itemcard_1_500, as.data.frame)))
df_501_1000 <- as.data.frame(do.call(rbind, lapply(itemcard_501_1000, as.data.frame)))
df_1001_1500 <- as.data.frame(do.call(rbind, lapply(itemcard_1001_1500, as.data.frame)))
df_1501_1913 <- as.data.frame(do.call(rbind, lapply(itemcard_1500_1913, as.data.frame)))
```

### Bind dataframes by rows
```
df_1_1000 <- rbind(df_1_500, df_501_1000)
df_1_1500 <- rbind(df_1_1000, df_1001_1500)
df_1_1913 <- rbind(df_1_1500, df_1501_1913)
df <- df_1_1913
```

### Remove "\r\n"
```
orgs1 <- trimws(gsub("[\r\n]", "", df$`X[[i]]`))
df$`X[[i]]` <- orgs1
colnames(df) <- "x"
```

### Separate into "org" (organization name) and "id_level" (the rest of the scrapped data)
```
df1 <- df %>% separate(
  col = x,
  into = c("org", "id_level"),
  sep = ":",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn"
)
```

### Create new columns with SAM status and level (e.g., ML2, ML3, ML4, ML5)
```
df1$sam_status <- ifelse(grepl("with SAM",df1$id_level),'with SAM',
                         ifelse(grepl("without SAM", df1$id_level), 'without SAM', 'something_else'))
                         
df1$level_status <- ifelse(grepl("ML3",df1$id_level),'ML3',
                           ifelse(grepl("ML2",df1$id_level),'ML2',
                                  ifelse(grepl("ML5",df1$id_level),'ML5',
                                         ifelse(grepl("ML4",df1$id_level),'ML4', 'something_else'))))
```

### Remove out "ID" from column with organization name
```
df2 <- df1 %>% separate(
  col = org,
  into = c("org", "null"),
  sep = "ID",
  remove = TRUE,
  convert = FALSE,
  extra = "warn",
  fill = "warn"
)

df2$null <- NULL
```

### Add CMMI-SVC or -DEV (optional)
```
df2$cmmi_type <- ifelse(grepl("CMMI-SVC",df2$id_level),'CMMI-SVC',
                         ifelse(grepl("CMMI-DEV", df2$id_level), 'CMMI-DEV', 'something_else'))

df2 <- df2[-1369,] # weird company, no relevant data
```

### Fix ids to add (optionality TBD)
```
df2$id <- trimws(df2$id)
id_extracted <- str_extract_all(df2$id, "0.|1.|2.|3.|4.|5.|6.|7.|8.|9.")
id_str <- trimws(id$id)
df2$id_ex <- id_ex
id_ex_v2 <- trimws(gsub("#first term ends in space, . , or - ", "", df2$id_ex))
```

### Select organization name, SAM status, ML status, and CMMI type
```
cmmi <- df2 %>% select(org, sam_status, level_status, cmmi_type)
```
                             
### Select organization with SAM and >= ML3
```
cmmi_sam_ml3 <- cmmi %>% filter(grepl("with SAM", cmmi$sam_status), grepl(c("ML3"), cmmi$level_status))
cmmi_sam_ml4 <- cmmi %>% filter(grepl("with SAM", cmmi$sam_status), grepl(c("ML4"), cmmi$level_status))
cmmi_sam_ml5 <- cmmi %>% filter(grepl("with SAM", cmmi$sam_status), grepl(c("ML5"), cmmi$level_status))

cmmi_final <- rbind(cmmi_sam_ml3, cmmi_sam_ml4, cmmi_sam_ml5)
```

### Export
```
library(writexl)
write_xlsx(cmmi_final, "cmmi_2021_sam_ML3_ML4_ML5_world.xlsx")
```



