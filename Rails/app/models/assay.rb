#
# Copyright 2012 The Board of Regents of the University of Wisconsin System.
# Contributors: Jason Shao, James McCurdy, Enhai Xie, Adam G.W. Halstead,
# Michael H. Whitney, Nathan DiPiazza, Trey K. Sato and Yury V. Bukhman
#
# This file is part of GCAT.
#
# GCAT is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# GCAT is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with GCAT.  If not, see <http://www.gnu.org/licenses/>.
#
require 'zip'
# require 'zip/zipfilesystem'
require 'fileutils'
include FileUtils
require_relative 'DataPassingError'

# single-plate timestamp
SECONDS = "1/3600".to_r.to_f
MAX_FILE_SIZE = 10000000.0

class Assay

  
  include ActiveModel::Validations
  include ActiveModel::Conversion
  extend ActiveModel::Naming
  attr_accessor :input_file, :blank_value, :blank_value_input, :start_index, :remove_points, :remove_jumps, :plate_type,
  :plate_dimensions_row, :plate_dimensions_column, :timestamp_format, :growth_threshold, :layout_file,:filename,:content_type, :model, :loess_input, :console_out, :specg_min,
  :specg_max, :totg_min, :totg_max, :totg_OD_min, :totg_OD_max, :lagT_min, :lagT_max,:transformation, :transformation_input, :area_start_hour, :area_end_hour,
  :uploaded_files_directory, :generated_files_directory, :input_file_path, :layout_file_path

  # (1) Validation of input data file
  validates_presence_of :input_file, :message => '- No input file was specified.'
  #Either look for .csv as file extension, or in mime type (content_type). No need to validate if there is not filename
  #validates_format_of :filename, :with => %r{\.(csv|xlsx)$}i, :message => "- You can only upload csv and xlsx files.", :unless => lambda { self.input_file == nil }
  validates_format_of :filename, :with => %r{\.(csv)$}i, :message => "- You can only upload csv files.", :unless => lambda { self.input_file == nil }
  
  def filename
    unless self.input_file.nil?
      self.filename = self.input_file.original_filename
    end
  end
  
  def content_type
    unless self.input_file.nil?
      self.content_type = self.input_file.content_type
    end
  end
  
  # (2)Validation of transformation
  # if user input is chosen, the user should enter a valid Delta value (A Real Number)
  validates_presence_of :transformation_input,  :if => :user_input_r_value?, :message => '- Please Enter Your Delta Value.'
  validates_numericality_of :transformation_input, :if => :user_input_r_value? , :greater_than_or_equal_to => 0 , :message => '- Invalid value for Delta. Please Enter a positive real number.'
  def user_input_r_value?
    transformation == "-1"
  end
  
  # (3) Validation of OD blank value
  # if user input is chosen,, the user should enter a valid OD blank value(A Real Number)
  #validates_inclusion_of :blank_value, :in => %w( default user  ), :message => '- Invalid blank value. Please choose one of options'
  validates_presence_of :blank_value_input,  :if => :user_input?, :message => '- Please Enter Your OD Blank Value.'
  validates_numericality_of :blank_value_input,:if => :user_input?,  :message => '- Invalid OD blank value. Please Enter A Real Number.'
  def user_input?
    blank_value == "user"
  end
  
  
  # (4) Validation of start_index
  # if user does not enter anything, the system uses the default value start_index = 2
  validates_format_of :start_index,:with => /^[0-9\s]*$/i, :unless => :default_value?,:message => '- Invalid value for start index. Please Enter A Positive Integer Number'
  def default_value?
    start_index == '' 
  end
  
  
  # (5) Validation of remove_points
  # if user does not enter anything, the system uses the default value remove_points = 0, that is an empty list
  #validates_format_of :remove_points,:with => /^[0-9 \,\s]*$/i, :unless => :remove_points_default_value?, :message => '- Please Enter a comma-separated list of points. Example: 2,3,4,5 (Positive Integer Number)'
  validates_format_of :remove_points,:with => /^(\d|\d+\s*,\s*)*$/i, :unless => :remove_points_default_value?, :message => '- Please Enter a comma-separated list of points. Example: 2,3,4,5 (Positive Integer Number)'
  def remove_points_default_value?
    remove_points == '' 
  end
  
  # (6) Validation of growth threshold
  validates_numericality_of :growth_threshold, :message => '- Please enter a number.'
  
  #validate plate dimensions
  #v1 not including custom plate dimensions
  #validates :plate_dimensions_column, :numericality => { :only_integer => true, :greater_than => 0}
  #validates :plate_dimensions_row, :numericality => { :only_integer => true, :greater_than => 0 }
  
  # (7) validate inoculation timepoint
  validates :start_index, :numericality => { :only_integer => true, :greater_than => 0, :message => '- Invalid value for start index. Please Enter A Positive Integer Number'}
  
  # (8) validate heatmap ranges
#validates_numericality_of :blank_value_input,:if => :user_input?,  :message => '- Invalid OD blank value. Please Enter A Real Number.'
  validates_numericality_of :totg_min, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :totg_max, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :totg_OD_min, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :totg_OD_max, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :specg_min, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :specg_max, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :lagT_min, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :lagT_max, :if => :user_input?, :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'

  validates_numericality_of :area_start_hour,  :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validates_numericality_of :area_end_hour,  :allow_blank => true, :greater_than_or_equal_to => 0, :message => '- Please Enter a positive real number.'
  validate :area_under_curve

  # Validates the area under curve start and end as they are mutually dependent.
  # Valid conditions:
  #     area_start_hour = nil, area_end_hour == nil
  #     area_start_hour >=0, area_end_hour == nil
  #     area_start_hour < area_end_hour, >=0, area_end_hour > 0
  # Invalid conditions:
  #     area_start_hour >= area_end_hour
  #     area_start_hour > 0, area_end_hour = nil
  # Note that negativity of these numbers is tested above in 'validates_numericality_of' statements.
  def area_under_curve
    #gy. Oct9
    self.area_start_hour ||= ''
    self.area_end_hour ||= ''

    test_area_start_hour = area_start_hour != '' ? self.area_start_hour.to_f : nil
    test_area_end_hour = area_end_hour != '' ? self.area_end_hour.to_f : nil
    
    if test_area_end_hour
    puts 'enter validation function'
      if test_area_end_hour <= 0
        self.errors.add(:area_end_hour, "must be greater than 0.")
      end
      if test_area_start_hour and test_area_start_hour >= test_area_end_hour
        puts 'enter core area'
        self.errors.add(:area_end_hour, "must be greater than start hour.")
        self.errors.add(:area_start_hour, "must be less than end hour.")
      end
    elsif test_area_start_hour and test_area_start_hour < 0
      self.errors.add(:area_start_hour, "must be greater than 0.")
    end


    # if not test_area_start_hour and not test_area_end_hour
    #   return
    # end
    # if not test_area_end_hour and test_area_start_hour >=0
    #   return
    # end
    # if not test_area_start_hour and test_area_end_hour > 0
    #   return
    # end


  end


  def initialize(attributes = {})
    attributes.each do |name, value|
      send("#{name}=", value)
    end
  end

  def persisted?  #
    false
  end

  def parse_form_params
    
    # (1) input data file
    
    # (2) transformation. N value (A Real Number)
    if self.transformation == '-1'
      self.transformation =  Float(self.transformation_input)
    else
      self.transformation = self.transformation.to_f
    end

    # Soothing parameter for growth curve model. Applied for Loess model only.
    if self.model == '-1' and self.loess_input != ""
        self.loess_input = Float(self.loess_input)
    end

    # (3) blank value (A Real Number)
    if self.blank_value == 'default'
      self.blank_value = nil
    elsif self.blank_value == 'zero'
      self.blank_value = 0
    else
      self.blank_value = Float(self.blank_value_input)
    end

    # (4) start index (A Positive Integer Number)
    if 
      start_index == ''
      self.start_index = 1
    else  
      self.start_index.gsub(/\s+/, "")  # remove white spaces
      self.start_index = self.start_index.to_i
    end
    
    # (5) remove points [a space-separated list of points. Example: 2 3 4 5 (Positive Integer Number)]
    self.remove_points = self.remove_points.gsub(/\r/,"")  # "Some text with a carriage return \r"
    #self.remove_points = self.remove_points.gsub(/\r\n/,"\n") # "Some text with a carriage return \r\n"
    self.remove_points = self.remove_points.gsub(/\s+/, "")  # remove white spaces

    ##collect! calls .to_i on each string in the array and replaces the string with the result of the conversion.
    self.remove_points = self.remove_points.split(',').collect! {|n| n.to_i}

    ## (6) remove jumps (true/false)
    if self.remove_jumps == 1
      self.remove_jumps = true
    else
      self.remove_jumps = false
    end

    ## (7) Heatmap values
    if (self.specg_min != '' && self.specg_max != '')
      self.specg_max = Float(self.specg_max)
      self.specg_min = Float(self.specg_min)
    end
    if (self.totg_min != '' && self.totg_max != '')
      self.totg_min = Float(self.totg_min)
      self.totg_max = Float(self.totg_max)
    end
    if (self.totg_OD_min != '' && self.totg_OD_max != '')
      self.totg_OD_min = Float(self.totg_OD_min)
      self.totg_OD_max = Float(self.totg_OD_max)
    end
    if (self.lagT_min != '' && self.lagT_max != '')
      self.lagT_max = Float(self.lagT_max)
      self.lagT_min = Float(self.lagT_min)
    end

    # Area under curve
    self.area_start_hour = self.area_start_hour != '' ? Float(self.area_start_hour) : nil
    self.area_end_hour = self.area_end_hour != '' ? Float(self.area_end_hour) : nil

############################################################################################

    return self

  end # end of parse_form_params method

  def pad_date(unit)
    unit.to_i < 10 ? "0" + unit.to_s : unit.to_s
  end

  def getUniqueID
    # uniqueID = Process.pid.to_s + "-" + Time.now.to_i.to_s
    #wanted the date to be easier to extract NWD 2/26/14
    today = Time.now
    
    uniqueID = today.year.to_s + pad_date(today.month) + pad_date(today.day) + "-" + today.to_i.to_s
  end
  
  def generate_directory_names
    uniqueID = getUniqueID
    # set working directories for uploaded and generated files
    @uploaded_files_directory = (Rails.root + "public/uploadedFiles/" + uniqueID).to_s
    @generated_files_directory = (Rails.root + "public/generatedFiles/" + uniqueID).to_s
  end

  #Raise Argument Error is file too large
  def generate_directory_and_move_files
    
    generate_directory_names

    # make directory and set permission 
    FileUtils.mkdir_p @uploaded_files_directory
    FileUtils.chmod 0777, @uploaded_files_directory
    FileUtils.mkdir_p @generated_files_directory
    FileUtils.chmod 0777, @generated_files_directory
   
    # upload input data file from where it locates into web server via uri/url
    fromfile = self.input_file
    @input_file_path = @uploaded_files_directory + fromfile.original_filename
      #supports testing scripts that directly uploads tempfile
    if fromfile.respond_to?(:tempfile)
      FileUtils.copy( fromfile.tempfile.path, @input_file_path )
    else 
      FileUtils.copy( fromfile.path, @input_file_path )
    end
    
    # upload layout data file from where it locates into web server via uri/url
    unless self.layout_file.nil?
      fromfile = self.layout_file
      @layout_file_path = @uploaded_files_directory + fromfile.original_filename
      if fromfile.respond_to?(:tempfile)
        FileUtils.copy( fromfile.tempfile.path, @layout_file_path )
      else
        FileUtils.copy( fromfile.path, @layout_file_path )
      end
    end
   
   raise DataPassingError, 'Input file too big', {:error_message => "Error: File too big. Maximum file size allowed is #{MAX_FILE_SIZE}/(10**6) MB", :path =>@input_file} unless File.size(@input_file_path) < MAX_FILE_SIZE
   raise DataPassingError, 'Layout file too big', {:error_message => "Error: File too big. Maximum file size allowed is #{MAX_FILE_SIZE}/(10**6) MB", :path =>@layout_file} unless File.size(@input_file_path) < MAX_FILE_SIZE
 
  end

  def bind_arguments_to_r
    # use web interface parsed parameters to call R function/library via Rinruby  
    R.eval ('library(GCAT)')
    R.assign "out.dir", @generated_files_directory
    # That is for Single Plate case. Need modification for multiple plate case
    #Use one set.constants call only!

    if self.plate_type == 's'
      R.assign 'single.plate', 'T'
      timestamp_format = SECONDS #self.timestamp_format.to_r.to_f
    elsif self.plate_type == 'm'
      R.assign 'single.plate', 'F'
      timestamp_format = ""+self.timestamp_format+""
    end
    
    #################for warringer data###########################################
    # used for custom dims 
    #R.assign 'plate.nrow', 10
    #R.assign 'plate.ncol', 20
    ##############################################################################
    
    R.assign 'time.input', timestamp_format
    #R.eval ("gcat.set.constants(time.format = #{timestamp_format}, plate.nrow = #{self.plate_dimensions_row}, plate.ncol = #{self.plate_dimensions_column})")

    # assign type of file to load.type; ext determines function call by R
    R.assign "load.type", "csv"
    ext = "csv" 


    # (1) input data file
    R.assign "file", @input_file_path

    first_rows = ["Well positions", "Destination plate name", "Plate ID"]
    begin
      file_row = ""
      File.open(@input_file_path) {|f| file_row = f.readline.split(",").first}
      unless first_rows.include?(file_row)
        raise DataPassingError,'Unknown file format', {:error_message => "Error: Unknown file format.", :path => input_file}
      end
    rescue
      #bad encoding try to validate in R
      first_rows.collect! {|r| r.gsub(" ", ".")} # convert to R format
      R.eval("test.out <- read.csv(file)")
      R.eval("first_entry <- names(test.out)[1]")     
=begin      
      puts 'first entry is'
      puts R.pull('names(test.out)[2]')
      puts R.first_entry
      puts R.file
      puts 'lala'
=end

      unless first_rows.include?(R.first_entry)
        raise DataPassingError,'Unknown file format', {:error_message => "Error: Unknown file format.", :path => input_file}
      end
    end
  

    # (2) transformation. N value (A Real Number)
    R.assign "add.constant", self.transformation
    # R.assign "add.constant", 0

    # (3) blank value (A Real Number)
    if (self.blank_value == nil)
      R.eval "blank.value <- NULL"
    elsif (self.blank_value == 0)
      R.eval "blank.value <- 0"
    else
      R.assign "blank.value", self.blank_value
    end

    # (4) start index (A Positive Integer Number).  Cannot be 1 if blank value is nil.
    if(self.blank_value == nil && start_index == 1)
      raise DataPassingError,'inoculation point error', {:error_message => "Error: inoculation timepoint cannot be 1 if using first OD reading as blank", :path => input_file}
    else 
      R.assign "start.index", self.start_index
    end
    
    # (5) remove points [a space-separated list of points. Example: 2,3,4,5 (Positive Integer Number)]
    R.assign "points.to.remove", self.remove_points
    
    R.assign  "growth.cutoff", self.growth_threshold
    
     if (self.layout_file_path ==nil)
      R.eval "layout.file <- NULL"
    else
      R.assign "layout.file", layout_file_path
    end
    
    #debugger
    #R.assign "plate.nrow", self.plate_dimensions_row
    #R.assign "plate.ncol", self.plate_dimensions_column
    
    ## (6) remove jumps (true/false)
    if (self.remove_jumps == true)
      R.eval "remove.jumps <- T"
    else
      R.eval "remove.jumps <- F"
    end
    # Using growth curve model. By default if this if block
    # is not taken then the Sigmund model is used.
    if (self.model == -1.to_s)
      R.assign 'use.loess', 'T'
      if (self.loess_input != "")
        R.assign 'smooth.param', self.loess_input
      else
        R.assign 'smooth.param', 0.1
      end
      R.assign 'use.linear.param', 'F'
    elsif (self.model == 0.to_s)
      # Currently not in use. May return someday... NWD 9/1
      #R.assign 'use.linear.param', 'T'
      R.assign 'use.linear.param', 'F' #must be false
      R.assign 'use.loess', 'F'
    else
      # Initialize values for growth curve models.
      R.assign 'use.loess', 'F'
      R.assign 'use.linear.param', 'F'
      R.assign 'smooth.param', 0.1 # default value
    end

    ## Heatmap values
    if (self.specg_max != '' && self.specg_min != '')
      R.assign 'specMin', self.specg_min
      R.assign 'specMax', self.specg_max
      R.eval "specRange <- c(specMin, specMax)"
    else
      R.eval 'specRange <- NA'
    end
    if (self.totg_min != '' && self.totg_max != '')
      R.assign 'totMin', self.totg_min
      R.assign 'totMax', self.totg_max
      R.eval "totalRange <- c(totMin, totMax)"
    else
      R.eval 'totalRange <- NA'
    end
    if (self.totg_OD_min != '' && self.totg_OD_max != '')
      R.assign 'totODMin', self.totg_OD_min
      R.assign 'totODMax', self.totg_OD_max
      R.eval "totalODRange <- c(totODMin, totODMax)"
    else
      R.eval 'totalODRange <- NA'
    end
    if (self.lagT_min != '' && self.lagT_max != '')
      R.assign 'lagT_min', self.lagT_min
      R.assign 'lagT_max', self.lagT_max
      R.eval "lagRange <- c(lagT_min, lagT_max)"
    else
      R.eval 'lagRange <- NA'
    end
    # Area under curve
    if self.area_start_hour != nil
      R.assign 'auc.start', self.area_start_hour
    else
      R.eval 'auc.start <- NULL'
    end
    if self.area_end_hour != nil
      R.assign 'auc.end', self.area_end_hour
    else
      R.eval 'auc.end <- NULL'
    end
  end


    
  def r_calculation
    begin
      generate_directory_and_move_files
    rescue DataPassingError => err
      return err.data
    end

    # Try to override stdout to redirect the console output.
    $stdout = File.new(@generated_files_directory + '/console.out', 'w')
    $stdout.sync = true 
    
    begin
      bind_arguments_to_r
    rescue DataPassingError => err
      return err.data
    end
    # This block evaluates the files (csv or xlsx, single.plate or multiple.plate)
    R.eval 'R_file_return_value <- gcat.analysis.main(
                        file, single.plate, layout.file, out.dir=out.dir, graphic.dir = out.dir, add.constant, blank.value, 
                        start.index, growth.cutoff, use.linear.param=use.linear.param, use.loess=use.loess, smooth.param=smooth.param, 
                        lagRange = lagRange, totalRange = totalRange, totalODRange = totalODRange, specRange = specRange, 
                        points.to.remove = points.to.remove, remove.jumps, time.input, plate.nrow = 8, 
                        plate.ncol = 12, input.skip.lines = 0, multi.column.headers = c("Plate.ID", "Well", "OD", "Time"), single.column.headers = c("","A1"), 
                        layout.sheet.headers = c("Strain", "Media Definition"), silent = T, verbose = F, return.fit = F, overview.jpgs = T,
                        auc.start=auc.start, auc.end=auc.end
            )'
    # good file returns a list of file path(length is more than 1), bad file returns error message string(array length = 1)
    print R.R_file_return_value
    
    R.eval ('R_array_return_length <- length(R_file_return_value)')
    unless  R.R_array_return_length == 1
      puts R.R_file_return_value, "\n"
      files = R.R_file_return_value  # returns a list of file path
      status = true
    else
      error_message = R.R_file_return_value
      if(error_message.include? "Error in <remove.points>")
        num_data_points = error_message.split("data has ").last.gsub("\n\n", "")
        error_message = "Invalid 'Points to ignore'. Please select a value in the range (1-#{num_data_points})."
      else
        # debugger
        console_message = error_message
        split_s = error_message.split(":")
        print split_s
        split_s.each {|x| error_message = x}
      end
      return {:error_message => error_message, :path => input_file, :console_msg => console_message}
    end
    
    # process generated files
    raise "no files generated" if files.empty?
    #search for "_overview.jpg files" from Array of files
    overviewFiles = ""
    pdfFile = ""
    txtFile = ""
    consoleOut = ""
    files_Array_Size = files.size - 1

    for i in 0..files_Array_Size
      if files[i].include? "_overview.jpg"
        overviewFiles = overviewFiles + files[i] +  "\n"
      end

      if files[i].include? "_plots"
        pdfFile = pdfFile + files[i] +  "\n"
      end

      if files[i].include? ".txt"
        txtFile = txtFile + files[i] +  "\n"
      end
    end
 

    unless pdfFile.empty?
      #pdfFile = pdfFile.sub!(Rails.root.to_s + '/public/', '') #sub!(pattern, replacement) will return nil if no substitutions were performed
      pdfFile = pdfFile.gsub(Rails.root.to_s + '/public/', '') #Oct. 04 2011 by Enhai
      pdfFile = pdfFile.gsub(/\r/,"")  # "Some text with a carriage return \r"
      pdfFile = pdfFile.gsub(/\r\n/,"\n") # "Some text with a carriage return \r\n"
      pdfFile = pdfFile.gsub(/\s+/, "")  # remove white spaces
    end

    unless txtFile.empty?
      #txtFile = txtFile.sub!(Rails.root.to_s + '/public/', '')#sub!(pattern, replacement) will return nil if no substitutions were performed
      txtFile = txtFile.gsub(Rails.root.to_s + '/public/', '') #Oct. 04 2011 by Enhai
      txtFile = txtFile.gsub(/\r/,"")  # "Some text with a carriage return \r"
      txtFile = txtFile.gsub(/\r\n/,"\n") # "Some text with a carriage return \r\n"
      txtFile = txtFile.gsub(/\s+/, "")  # remove white spaces
    end

    # build array named overviewFiles that contains "_overview.jpg files"
    overviewFiles = overviewFiles.split("\n")
   
    raise "no overview files generated" if overviewFiles.empty?

    if self.plate_type == 'm'
      zipfile = @generated_files_directory + "/multiplePlateAnalysis.zip"
    else
      zipfile = @generated_files_directory + "/singlePlateAnalysis.zip"
    end
    consoleOut = @generated_files_directory + "/console.out"
    self.console_out = consoleOut
    # create Zip files at current directory
    Zip::File.open(zipfile, Zip::File::CREATE) { |zf|
      files.each{|file| zf.add(File.basename(file), file)}
      #files.each{|file| zf.add(file.sub(out_dir_path + "/", ""), file))}
      #zf.add(File.basename(consoleOut), consoleOut)
    }
    #return results unless error
    #zip files, jpg of overviews, txt file for datagrid, pdf file
    
    
    {:status => status, :overviewFiles => overviewFiles, :zipfile => zipfile, :txtFile => txtFile, :pdfFile => pdfFile, :inputfile => input_file, :layout_file => layout_file, :model => self.model,
     :consoleout => consoleOut}
  end # end of r_calculation method

  
end # class Assay
