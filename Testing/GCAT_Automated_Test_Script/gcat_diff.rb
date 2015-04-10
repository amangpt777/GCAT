#!/usr/bin/env ruby

def usage rc, *args
    case rc
    when "argv"
        puts "Usage: ./gcat_diff.rb <gcat-test>/ <gcat-production>/"
        # puts "<gcat-test> is a directory of output files processed on gcat-test.glbrc"
        # puts "<gcat-production> is a directory holding output files processed on gcat.glbrc"
    when "names"
        puts "Error: both directories must contain output files with identical file names."
    when "dir"
        puts "Error: #{args.first} is not a directory."
    when "empty"
        puts "No files found to analyze."
    end
    puts "for help: ./gcat_diff.rb help"
    exit 0
end

def line
    80.times {print "#"}
    puts ""
end

def help
    line
    puts "# gcat_diff.rb\n#"
    puts "# This program was designed to verify that any changes made to gcat-test\n# have not changed the numeric values of the output data in any meaningful way.\n# gcat_diff is the script used to automate this process.\n# To use the script you must have R and ruby installed.\n# Once you are ready to run the comparison analysis follow these steps:\n# In a directory containing both script files\n# [gcat_diff.rb and compare_gcat_output.R]\n# create 2 new directories to store the output files.\n# One will be for files from gcat-test.glbrc.org\n# and the other will be for gcat.glbrc.org.\n# Place any number of output files desired into\n# these directories following this important convention.\n# See the GCAT manual for help on producing these files.\n# Note: The needed output files will be text format.\n# Each output file must be run on GCAT\n# using the same PARAMETERS and share the same NAME.\n# This way you can analyze many output files at once,\n# with each pair being run onproduction & test\n#  using the same parameters and compared in tandem.\n# \n# Nate DiPiazza, Karan Maini GLBRC 2014"
    line

    exit 1
end

if(ARGV.size == 1)
    usage "argv" unless ARGV.first == "help"
    help
end

usage "argv" unless ARGV.size == 2

ARGV.each {|arg| usage "dir", arg unless File.directory?(arg)}

#check the contents of the 2 dirs. files must have same filenames
paths_test = Dir.glob("#{ARGV[0]}/*").sort
filenames_test = paths_test.map {|p| File.basename(p)}
paths_prod = Dir.glob("#{ARGV[1]}/*").sort
usage "empty" unless((paths_test.size > 0) and (paths_prod.size > 0))
filenames_prod = paths_prod.map {|p| File.basename(p)}

usage "names" unless filenames_test == filenames_prod
#filenames_prod would work too. they are equal
filenames_test.each do |filename|
    # run the R script
    command = `Rscript compare_gcat_output.R #{ARGV[0]}/#{filename} #{ARGV[1]}/#{filename} #{filename}`
    # write R summary printouts to a file
    File.open("Comparison_Datasets/#{filename}", "w") {|f| f.puts command; f.close }
end