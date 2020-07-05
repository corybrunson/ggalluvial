# Raff's example

# read categorical discrete time series data
tableNSA <- read.fwf(file = "data-raw/tableNSA.txt",
                     widths = c(2, 10, 10, 10, 5, 12), sep = "",
                     header = TRUE, fileEncoding = "UTF-8")
# remove relative frequency field
tableNSA$a <- NULL
for (i in 1:3) {
  tableNSA[[i]] <- factor(tableNSA[[i]],
                          rev(c("Always", "Sometimes", "Never", "Missing")))
}
# convert NSA table to lode form
ggalluvial::is_alluvia_form(tableNSA, axes = 1:3, weight = "freq")
vaccinations <- ggalluvial::to_lodes_form(
  tableNSA,
  key = "survey", value = "response", id = "subject",
  axes = 1:3
)
# merge in survey dates
survey_dates <- data.frame(
  survey = levels(vaccinations$survey),
  start_date = as.Date(c("2010-09-22", "2015-06-04", "2016-09-27")),
  end_date = as.Date(c("2010-10-25", "2015-10-05", "2016-10-25"))
)
vaccinations <- merge(vaccinations, survey_dates, by = "survey")
# save
save(vaccinations, file = "data/vaccinations.rda")

# Dario's example

# read lode dataset and introduce factors
majors <- read.csv("data-raw/majorsDt.csv", stringsAsFactors = FALSE)
majors$X <- NULL
majors$curr <- factor(majors$curr, levels = unique(majors$curr))
names(majors) <- c("student", "semester", "curriculum")
# save
save(majors, file = "data/majors.rda")
