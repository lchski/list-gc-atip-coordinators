library(tidyverse)
library(rvest)

coordinators_page <- read_html("https://www.tbs-sct.gc.ca/ap/atip-aiprp/coord-eng.asp")

coordinators <- tibble(
  institution = coordinators_page %>%
    html_nodes("main.container > dl dt") %>%
    html_text,
  coordinator_raw = coordinators_page %>%
    html_nodes("main.container > dl dd") %>%
    html_text2 ## better linebreak handling
) %>%
  mutate(coordinator_raw = str_remove(coordinator_raw, fixed("\r "))) %>%
  mutate(coordinator_raw = str_remove(coordinator_raw, fixed("\r\nATIP Online Request"))) %>%
  separate(coordinator_raw, c("name", "title", "coordinator_raw"), sep = "\r", extra = "merge") %>%
  mutate(email = str_extract(coordinator_raw, "(.)*\\r$")) %>%
  mutate(phone = str_extract(coordinator_raw, "\n\r Telephone: (.)*\\n\\r")) %>%
  mutate(phone = str_remove(phone, fixed("\n\r Telephone: "))) %>%
  mutate(address = str_split(coordinator_raw, fixed("\n\r Telephone:"), n = 2, simplify = TRUE)[,1]) %>%
  select(-coordinator_raw) %>%
  mutate(across(.fns = trimws))

coordinators %>% write_csv("data/out/atip_coordinators.csv")
