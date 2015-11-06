# What's in this package

- R - R code.  Sub-folder GCAT contains the GCAT package
- Rails - Rails code
- Testing - testing code, data and documents, outside of any regression tests that may be incorporated directly into R packages or Rails applications 

# Getting started

First you will need to install Ruby and Ruby on Rails. I suggest using RVM to
install them. Documentation for RVM can be found at: http://rvm.io.

## You will need: 
- Ruby version 1.9.3p194 and Rails 3.2.15
- R version between 3.0.2 and 3.2.0.

The R package is in subfolder R. To install, do the following:
Open a terminal in the R folder

```bash
$  sudo R CMD REMOVE GCAT # do this if an older version of GCAT has been installed
$  sudo R CMD INSTALL GCAT
```

The rails application is in subfolder Rails.  It runs under Rails 3.2.15.  To run it locally using the default Rails WEBrick web server do the following:
Open a terminal in the Rails folder 

```
bash
$ bundle install
$ rails s
```

Open http://0.0.0.0:3000 in a web browser

