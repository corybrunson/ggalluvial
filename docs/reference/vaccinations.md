# Influenza vaccination survey responses

This data set is aggregated from three RAND American Life Panel (ALP)
surveys that asked respondents their probability of vaccinating for
influenza. Their responses were discretized to "Never" (0%), "Always"
(100%), or "Sometimes" (any other value). After merging, missing
responses were coded as "Missing" and respondents were grouped and
counted by all three coded responses. The pre-processed data were kindly
contributed by Raffaele Vardavas, and the complete surveys are freely
available at the ALP website.

## Usage

``` r
vaccinations
```

## Format

A data frame with 117 rows and 5 variables:

- `freq`:

  number of respondents represented in each row

- `subject`:

  identifier linking respondents across surveys

- `survey`:

  survey designation from the ALP website

- `start_date`:

  start date of survey

- `end_date`:

  end date of survey

- `response`:

  discretized probability of vaccinating for influenza

## Source

<https://alpdata.rand.org/>
