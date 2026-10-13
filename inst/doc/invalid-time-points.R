## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(mixtime)

## -----------------------------------------------------------------------------
# Valid: day 15 exists in every month
date("2021-01-15") + months(1L)

# Invalid: February 2021 has no day 31
datetime("2021-01-31 09:30:00") + months(1L)

## -----------------------------------------------------------------------------
# 2025 has no 29 February
date("2024-02-29") + years(1L)

# The ISO week year 2021 has no 53rd week
time_shift(yearweek("2020 W53"), years(1L, calendar = cal_isoweek))

## -----------------------------------------------------------------------------
x <- datetime("2025-10-04 02:30:00", tz = "Australia/Melbourne")
x + days(1L)

## -----------------------------------------------------------------------------
x + hours(24L)

## -----------------------------------------------------------------------------
datetime("2025-04-05 02:30:00", tz = "Australia/Melbourne") + days(1L)

## -----------------------------------------------------------------------------
date("2026-02-30")
datetime("2025-10-05 02:30:00", tz = "Australia/Melbourne")

## -----------------------------------------------------------------------------
x <- datetime("2021-01-31 09:30:00")

## -----------------------------------------------------------------------------
time_shift(x, months(1L), on_invalid = "clamp")

## -----------------------------------------------------------------------------
time_shift(x, months(1L), on_invalid = "previous")
time_shift(x, months(1L), on_invalid = "next")

## -----------------------------------------------------------------------------
time_shift(date("2021-01-31"), months(1L), on_invalid = "previous")
time_shift(date("2021-01-31"), months(1L), on_invalid = "next")

## -----------------------------------------------------------------------------
xc <- datetime("2021-01-31 09:30:00", discrete = FALSE)
time_shift(xc, months(1L), on_invalid = "previous") ==
  time_shift(xc, months(1L), on_invalid = "next")

## -----------------------------------------------------------------------------
time_shift(x, months(1L), on_invalid = "overflow")

## -----------------------------------------------------------------------------
time_shift(x, months(1L), on_invalid = "reject")

## ----error = TRUE-------------------------------------------------------------
try({
time_shift(x, months(1L), on_invalid = "error")
})

## -----------------------------------------------------------------------------
x <- datetime("2025-10-04 02:30:00", tz = "Australia/Melbourne")
time_shift(x, days(1L), on_invalid = "clamp")
time_shift(x, days(1L), on_invalid = "previous")
time_shift(x, days(1L), on_invalid = "next")
time_shift(x, days(1L), on_invalid = "overflow")

## -----------------------------------------------------------------------------
seq(date("2021-01-31"), by = "1 month", length.out = 4)

## -----------------------------------------------------------------------------
seq(date("2021-01-31"), by = "1 month", length.out = 4, on_invalid = "clamp")
seq(date("2021-01-31"), by = "1 month", length.out = 4, on_invalid = "next")

## -----------------------------------------------------------------------------
date("2021-01-31") + months(1L)

options(mixtime.on_invalid = "next")
date("2021-01-31") + months(1L)
seq(date("2021-01-31"), by = "1 month", length.out = 4)

## ----error = TRUE-------------------------------------------------------------
try({
options(mixtime.on_invalid = "error")
date("2021-01-31") + months(1L)
})

## ----echo = FALSE-------------------------------------------------------------
options(mixtime.on_invalid = NULL)

## -----------------------------------------------------------------------------
date("2021-01-31") + months(1L) + months(1L)
date("2021-01-31") + months(2L)

## -----------------------------------------------------------------------------
date("2021-01-31") + months(1L) - months(1L)

## -----------------------------------------------------------------------------
y <- datetime(c("2021-01-29 23:00:00", "2021-01-31 08:00:00"))
time_shift(y, months(1L), on_invalid = "clamp")
time_shift(y, months(1L), on_invalid = "previous")

time_shift(date(c("2021-01-31", "2021-02-01")), months(1L), on_invalid = "overflow")

