# Raff's example

# read categorical discrete time series data
tableNSA <- read.fwf(file = "data-raw/tableNSA.txt",
                     widths = c(2, 10, 10, 10, 5, 12), sep = "",
                     header = TRUE, fileEncoding = "UTF-8")
for (i in 1:3) {
  tableNSA[[i]] <- factor(tableNSA[[i]],
                          rev(c("Always", "Sometimes", "Never", "Missing")))
}
# convert NSA table to lode form
ggalluvial::is_alluvial(tableNSA, axes = 1:3, weight = "freq")
vaccinations <- ggalluvial::to_lodes(
  tableNSA,
  key = "survey", value = "response", id = "subject",
  axes = 1:3
)
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
