## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(mixtime)

## -----------------------------------------------------------------------------
format(date(Sys.Date()), format = "{lin(year(1L))}-{cyc(month(1L), year(1L))}")

## -----------------------------------------------------------------------------
format(date(Sys.Date()), format = "{lin(year)} {cyc(month, year, label = TRUE, abbreviate = TRUE)}")

## -----------------------------------------------------------------------------
format(linear_time(Sys.Date(), chronon = cal_gregorian$month(1L)))

## -----------------------------------------------------------------------------
# Default format for a second-resolution chronon is "YYYY-MM-DD HH:MM:SS"
fmt_time <- chronon_format_linear(cal_gregorian$second(1L))
mel <- "Australia/Melbourne"
fmt_time_mel <- "({cyc(hour(1L, tz=mel), day(1L, tz = mel))}:{cyc(minute(1L, tz=mel), hour(1L, tz = mel))}:{cyc(second(1L, tz=mel), minute(1L, tz = mel))} {tz(.time, mel)})"
format(
  datetime(Sys.time(), tz = "UTC"), 
  format = paste(fmt_time, "{tz(.time)}", fmt_time_mel)
)

## -----------------------------------------------------------------------------
fmt_date  <- "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}"
fmt_lunar <- "{with(cal_time_lunar, cyc(phase, month, label = TRUE, type = \"emoji\"))}"
format(date(Sys.Date()), format = paste(fmt_date, fmt_lunar))

## -----------------------------------------------------------------------------
# YYYY-MM-DD format for a Gregorian date
format(date(Sys.Date()), format = "{lin(year)}-{cyc(month, year)}-{cyc(day, month)}")
format(Sys.Date(), format = "%Y-%m-%d")

## -----------------------------------------------------------------------------
# YYY-DDD format for a Gregorian date
format(date(Sys.Date()), format = "{lin(year)}-D{cyc(day, year)}")
format(Sys.Date(), format = "%Y-D%j")

## -----------------------------------------------------------------------------
# YYYY-WW-DD format for a ISO week date
format(
  date(Sys.Date(), calendar = cal_isoweek),
  format = "{lin(year)}-W{cyc(week, year)}-{cyc(day, week, label = TRUE)}"
)
format(Sys.Date(), format = "%G-W%V-%a")

