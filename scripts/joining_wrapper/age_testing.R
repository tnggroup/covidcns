```{r}
test_for_age <- data_joined %>%
  select(
    "ID",
    
    # Age
    "dem.dob_age",
    "dem.how_old_are_you_now.txt",
    
    # Gender
    "dem.what_gender_do_you_identify_with",
    "dem.what_gender_do_you_identify_with_numeric",
    
    # Language
    "dem.is_english_your_first_language",
    "dem.what_is_your_first_language.txt",
    "dem.what_is_your_first_language_numeric",
    "dem.is_english_your_first_language_numeric",
    
    # Education
    "dem.highest_education",
    "dem.highest_education_numeric",
    
    # impact
    "impact.fulltime_employed",
    "impact.unemployed",
    "impact.stayathome_parent_or_carer",
    "impact.contract_or_freelance_work",
    "impact.receiving_state_income",
    "impact.student_",
    "impact.prefer_not_to_say",
    "impact.parttime_employed",
    "impact.zerohours_contract",
    "impact.selfemployed",
    "impact.small_buisness_owner",
    "impact.retired",
    "impact.student_.1"
  ) %>%
  filter(is.na(dem.dob_age))
```

```{r}
sum(!is.na(test_for_age[,2:24]))
```
```{r}
test_for_age[!is.na(test_for_age)]
```


```{r}
missing_ages <- unlist(test_for_age$ID)
missing_ages
```

```{r}
saveRDS(object = missing_ages,
        file = "scripts/missing_age_ids.rds")
```

```{r}
sum(rowSums(is.na(data_joined[,4:1374]))>=1300)
```

```{r}
na_counts <- colSums(is.na(data_joined))
na_percs <- (na_counts/nrow(data_joined))*100
na_check_tibble <- rbind(na_counts, na_percs)
rownames(na_check_tibble) <- c("NA count", "NA percentage")
na_check_tibble <- as_tibble(na_check_tibble)
median(na_percs)
```


```{r}
Amelia::missmap(data_joined)
```


```{r}
View(data_joined %>% filter(ID %in% missing_ages))
```







```{r change incorrect ids}
swapped_ids <- readxl::read_xlsx(path = paste0(ilovecovidcns, "/Swapped IDs.xlsx"),
                                 sheet = "Sheet1")

colnames(swapped_ids) <- c("correct_ID", "incorrect_ID")

test_df <- data_joined[,1:2]

swapping_rows <- c()

for (i in swapped_ids$incorrect_ID) {
  swapping_rows <- append(x = swapping_rows, values = which(data_joined$ID == i))
}

swapped_ids <- cbind(swapped_ids, swapping_rows)

test_df.1 <- test_df

for (i in 1:length(swapped_ids)) {
  test_df.1[swapped_ids[i,3], 1] <- swapped_ids[i,1]
}

```

```{r}
subset(test_df, ID %in% swapped_ids$incorrect_ID)
```

```{r}
test_df.1[unlist(swapped_ids$swapping_rows),]
```

```{r}
swapped_ids
```
