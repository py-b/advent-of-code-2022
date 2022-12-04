# Build an aoc package to work through your solutions
#
# README: each step of the dev files is optional, and you don't have to
# fill every dev script before getting started.
# 01_start.R should be filled at start.
# 02_dev.R should be used to keep track of your development during the project.
# Dev file templates based on golem package dev files
# (https://github.com/ThinkR-open/golem).
#
########################################
#### CURRENT FILE: ON START SCRIPT #####
########################################

## Set {aoc} options ---
## Tell the aoc package which defaults to use
## aoc is a usethis-style functions for Advent of Code puzzles
## (see dev/02_dev.R for dependencies)
# Edit .RProfile and add options lines
usethis::edit_r_profile(scope = "project")
# example: options(aoc.year = 2017, aoc.package = "awesomeadvent2017")

## Create Common Files ----
## See ?usethis for more information
usethis::use_gpl_license(version = 3)  # You can set another license here
usethis::use_readme_rmd( open = FALSE )

## Use git ----
usethis::use_git()

# You're now set! ----

# go to dev/02_dev.R
rstudioapi::navigateToFile( "dev/02_dev.R" )
