# Gesture-Study 2
### Codename Glyphgrid

Understanding human word-order preferences with gesture/pantomime paradigms. See the OSF project repository () and preprint () for the full description of the project.

This repository contains all stimuli, data, and analyses for study 2.

## alldata.csv, alldata_columns.txt

A tidy version of the data; containing the order of moved items people passed to the alien on each trial, plus
with column descriptions. 

## experiment script/

Code used to run the experiment on Amazon Mechanical Turk. These scripts were created with [PsiTurk][https://psiturk.org/] and [JSPsych][http://www.jspsych.org/] (using a study block modified heavily from the free-sort component they provide). The stimuli themselves are in the mov/ folder, but can also be directly accessed over at the [stimuli component][https://osf.io/n8bn9/]. 

## GlyphGridAnalysis.R, GlyphGridCleaning.R

The R files; reproduces the entire analysis pipeline from the raw data. The raw data is a big ball of json inside the experiment script directory; running GlyphGridCleaning.R reproduces the tidy data in alldata.csv.  

GlyphGridAnalysis.R reads from that file and creates all reported stats & graphs. Lines 0 - 175 read in the raw data and produce the final word orders we analyze (simplifying strings like SVOV). For this task we use the *first* three unique glyphs people passed because this reflects their initial choices; unlike the gesture task people had no way to indicate a 'start over', and once e.g. the Subject glyph had been passed it could not be re-passed (ala gesture referent repetition.)  

Look how much easier to read this is than Study 1. Hooray for the [tidyverse][https://blog.rstudio.org/2016/09/15/tidyverse-1-0-0/]!

## initial summaries produced by masm/

Legacy folder for completeness, contains initial descriptive stats

## Reproducibility notes

If you open Gesture-GlyphGridRepository.Rproj in Rstudio, packrat should prompt you to install + fetch correct versions of all the necessary packages. 

Rstudio Version 0.98.1102

> R.Version()
$platform
[1] "x86_64-apple-darwin13.4.0"

$arch
[1] "x86_64"

$os
[1] "darwin13.4.0"

$system
[1] "x86_64, darwin13.4.0"

$status
[1] ""

$major
[1] "3"

$minor
[1] "1.2"

$year
[1] "2014"

$month
[1] "10"

$day
[1] "31"

$`svn rev`
[1] "66913"

$language
[1] "R"

$version.string
[1] "R version 3.1.2 (2014-10-31)"

$nickname
[1] "Pumpkin Helmet"