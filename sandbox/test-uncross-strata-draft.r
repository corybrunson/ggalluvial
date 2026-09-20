# Interactive checks of the uncross_strata() function's order options on
# ggalluvial's own datasets and on a Titanic-derived lodes example with counts.
# Run in an R console: the stratum levels from each option print automatically
# as the assigned objects are evaluated.

if (file.exists("uncross-strata-draft.r")) {
  source("uncross-strata-draft.r")
} else {
  source("sandbox/uncross-strata-draft.r")
}

suppressPackageStartupMessages({
  library(ggalluvial)
  library(dplyr)
  library(tidyr)
  library(patchwork)
})

# --- majors (no weight column; semester/curriculum/student names) -------------
data(majors)
is.factor(majors$curriculum)   # FALSE: this exercises the "factor, if not
                               # already a factor" branch

set.seed(1)
m1 <- uncross_strata(
  majors, key = "semester", value = "curriculum",
  id = "student", order = "forward"
)
levels(m1$curriculum)          # default `start`: first axis, `forward`
set.seed(1)
m1a <- uncross_strata(
  majors, key = "semester", value = "curriculum",
  id = "student", order = "forward", start = 1
)
stopifnot(identical(levels(m1$curriculum), levels(m1a$curriculum)))
set.seed(1)
m3 <- uncross_strata(
  majors, key = "semester", value = "curriculum",
  id = "student", order = "mean_rank"
)
levels(m3$curriculum)          # can differ from the others, because some
                               # curricula appear at more than one semester

stopifnot(is.factor(m1$curriculum), is.factor(m3$curriculum))
stopifnot(identical(colnames(m1), colnames(majors)),
          identical(colnames(m3), colnames(majors)))

mp0 <- majors |> 
  ggplot(aes(x = semester, stratum = curriculum, alluvium = student)) +
  geom_alluvium() +
  geom_stratum(aes(fill = curriculum))
mp1 <- m1 |> 
  ggplot(aes(x = semester, stratum = curriculum, alluvium = student)) +
  geom_alluvium() +
  geom_stratum(aes(fill = curriculum))
mp3 <- m3 |> 
  ggplot(aes(x = semester, stratum = curriculum, alluvium = student)) +
  geom_alluvium() +
  geom_stratum(aes(fill = curriculum))
mp0 + mp1 + mp3

# --- vaccinations (has a freq weight column) ----------------------------------
data(vaccinations)

set.seed(1)
v1 <- uncross_strata(
  vaccinations, key = survey, value = response,
  id = subject, weight = freq, order = "forward"
)
levels(v1$response)
set.seed(1)
v3 <- uncross_strata(
  vaccinations, key = survey, value = response,
  id = subject, weight = freq, order = "mean_rank"
)
levels(v3$response)            # each response occurs at one survey, so forward
                               # (start = 1, surveys 1,2,3) agrees with
                               # mean_rank

vp0 <- vaccinations |> 
  ggplot(aes(x = survey, stratum = response, alluvium = subject, y = freq)) +
  geom_alluvium() +
  geom_stratum(aes(fill = response))
vp1 <- v1 |> 
  ggplot(aes(x = survey, stratum = response, alluvium = subject, y = freq)) +
  geom_alluvium() +
  geom_stratum(aes(fill = response))
vp3 <- v3 |> 
  ggplot(aes(x = survey, stratum = response, alluvium = subject, y = freq)) +
  geom_alluvium() +
  geom_stratum(aes(fill = response))
vp0 + vp1 + vp3

# --- Titanic counts in lodes form (via to_lodes_form on sorted wide) ----------
titanic <- as.data.frame(Titanic)
titanic_wide <- wompwomp::sort_to_uncross(
  titanic, cols = c("Class", "Sex", "Age"), wt = "Freq", method = "none"
)
titanic_lodes <- to_lodes_form(titanic_wide, axes = c("Class", "Sex", "Age"))
titanic_lodes$y <- titanic_wide$Freq[
  match(titanic_lodes$alluvium, rownames(titanic_wide))
]

set.seed(1)
t1 <- uncross_strata(titanic_lodes, order = "forward")
levels(t1$stratum)
set.seed(1)
t3 <- uncross_strata(titanic_lodes, order = "mean_rank")
levels(t3$stratum)             # every stratum lives at a single axis, so all
                               # the order options agree (forward, start = 1,
                               # scans Class,Sex,Age)

stopifnot(
  is.factor(t1$stratum), is.factor(t3$stratum)
)

tp0 <- titanic_lodes |> 
  ggplot(aes(x = x, stratum = stratum, alluvium = alluvium, y = Freq)) +
  geom_alluvium() +
  geom_stratum(aes(fill = stratum))
tp1 <- t1 |> 
  ggplot(aes(x = x, stratum = stratum, alluvium = alluvium, y = Freq)) +
  geom_alluvium() +
  geom_stratum(aes(fill = stratum))
tp3 <- t3 |> 
  ggplot(aes(x = x, stratum = stratum, alluvium = alluvium, y = Freq)) +
  geom_alluvium() +
  geom_stratum(aes(fill = stratum))
tp0 + tp1 + tp3
