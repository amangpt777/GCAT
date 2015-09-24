include ApplicationHelper
include TableBuilder

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
class AssaysController < ApplicationController

  def new
    @assay= Assay.new
  end

  def create 
    #Lazy mass assign from form params
    @assay = Assay.new(params[:assay])

    if @assay.valid?
    
      # Add the calls to methods to parse the form input in @assay and execute R script.
      # return the results from R into a new instance variable and display results via show template
      
      #reassign some parameters. Validate on form input params or after manipulations?
      @assay = @assay.parse_form_params
      #debugger
      
      #execute R calculation and assign instance variable to display     
      @result = @assay.r_calculation

      # no need to keep storing old data
      
      #temp fix for pdfFiles see ticket #424. string of pdfFiles returned rather than array
      unless @result[:pdfFiles].nil?
       @result[:pdfFiles] = @result[:pdfFile].split('pdf')
       @result[:pdfFiles].collect{|element| element + 'pdf'}
      end
      
      if  ( @result.has_key? :error_message )  #h.has_key?("a") # @result.has_key? :error_message
        flash.now[:error] = @result[:error_message] #.join("\n")
        #do not allow bad guys to fill disc space with invalid files
        FileUtils.rm @result[:path]
        # render :action => 'inputfile_error_message'
        @error_msg = @result[:error_message]
        @console_msg = @result[:console_msg]
        render :action => 'inputfile_error_message'
        
      else
        # parse the output text file into a hash in order to create a table in Assays#show
        @table = output_table(@result, !@result[:layout_file].nil?)
        remove_old_files(relative_path(@result[:zipfile]))
        flash.now[:notice] = "Your assay processed!\nPlease click on any of plate diagrams to save your results to a zip archive."  
        render :action => 'show'
      end    
    else

      render :action => 'new'
    end  
  end  


end
