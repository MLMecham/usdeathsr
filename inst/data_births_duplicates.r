# 1969-71 share the same format as 1969
data_births_1970 <- data_births_1969
data_births_1971 <- data_births_1969

# 1972-77 share the same format as 1972
data_births_1973 <- data_births_1972
data_births_1974 <- data_births_1972
data_births_1975 <- data_births_1972
data_births_1976 <- data_births_1972
data_births_1977 <- data_births_1972

usethis::use_data(
    data_births_1970,
    data_births_1971,
    data_births_1973,
    data_births_1974,
    data_births_1975,
    data_births_1976,
    data_births_1977,
    overwrite = TRUE
)
