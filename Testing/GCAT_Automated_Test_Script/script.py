import sys
import os
import glob

def usage():
	print 'python <script_name>.py <directory_path_1> <directory_path_2>'
	print 'directory_path_1 --> GCAT2 and directory_path_2 --> GCAT3'
	print 'Eg: python ./script.py ./GCAT2 ./GCAT3'
	 

def main():
	first_dir_path = sys.argv[1]
	second_dir_path = sys.argv[2]
	
	files1=[]
	files2=[]
	parameter_list=[]	

	for root,dirs,files in os.walk(first_dir_path):
		files1.extend(files)
	print 'Files in GCAT2:'
	print files1
	print '\n'

	for root,dirs,files in os.walk(second_dir_path):
		files2.extend(files)
	print 'Files in GCAT3:'
	print files2
	print '\n'

	print 'Common Files:'
	print [f for f in files2 if f in files1]
	print '\n'

	for f in files2:
		if f in files1: 
			parameter_list.append(f[:-4])
	# print parameter_list		
		

	# using glob
	# parameter_list = ['Media_Background','OD_Transform', 'Inoculation', 'Points_Ignored', 'Growth_Threshold']
	for parameter in parameter_list:
		file_1 = glob.glob(first_dir_path+"/"+parameter + "*.txt")
		file_2 = glob.glob(second_dir_path+"/"+parameter + "*.txt")
		print file_1
		print file_2
		if len(file_1)==1 and len(file_2)==1:
			# run it on command line
			command = "Rscript compare_gcat_output.R "+ file_1[0] + " " + file_2[0] + " " + parameter
			print command			
			os.system(command + '>' + './Comparison_Datasets/' + parameter + '.txt') # run it on command line			
			print 'done'
		else: 
			print 'Multiple files found for the parameter' + parameter


if __name__ == '__main__':
	# 2 arguments required other than script name: 
	# script name, dir_path_1, dir_path_2
	# If insufficent command line arguments are passed, usage must be printed
	if len(sys.argv) != 3:
		usage()
	# else continue with execution of script
	else:	
		main()
