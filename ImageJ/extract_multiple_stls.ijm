script_version = "0.9.0000";

requires("1.39l");
if (isOpen("Log")) { 
     selectWindow("Log"); 
     run("Close"); 
} 
if (isOpen("Results")) { 
     selectWindow("Results"); 
     run("Close"); 
}
if (isOpen("3D Viewer")) { 
     selectWindow("3D Viewer"); 
     run("Close"); 
}
//while (nImages>0) { 
//          selectImage(nImages); 
//          close(); 
//}

plugins = getDirectory("plugins");
unix = '/plugins/';
windows = '\\plugins\\';

if(endsWith(plugins, unix)){
	print("Running on Unix...");
	dir_sep = "/";
}
else if(endsWith(plugins, windows)){
	print("Running on Windows...");
	dir_sep = "\\";
}

//get source dir from user and define other directories
parent_dir_path = getDirectory("Select source Directory");
parent_dir_name = File.getName(parent_dir_path);

log_dir_path = parent_dir_path+dir_sep+'STL_extraction_log';
while(File.exists(log_dir_path)){
	waitForUser("Please delete or rename the ROI folder ("+log_dir_path+")!");
}

filelist = getFileList(parent_dir_path);

if (endsWith(filelist[0], ".nrrd")){
	print("Trying to open nrrd file in "+parent_dir_path+dir_sep+"...");
	//File.openSequence(parent_dir_path);
	run("Nrrd ...", "load="+parent_dir_path+dir_sep+filelist[0]);
} else {
	exit("No *.nrrd file found.");
}

title_original = getTitle();
setTool("rectangle");

Stack.getDimensions(width,height,channels,slices,frames);
getPixelSize(unit, px_size, ph, pd);

run("Select None");
setSlice(slices/2);

waitForUser("Threshold", "Find appropriate threshold level.");
	Dialog.create("Threshold");
	Dialog.addMessage("___________________________________");
	Dialog.addMessage("Define appropriate threshold level.");
	Dialog.addNumber("Threshold :", 100);
	Dialog.addMessage("___________________________________");
	Dialog.addMessage("Define desired ROI stack size.");
	Dialog.addNumber("ROI stack size (MB):", 100);
	Dialog.addMessage("We have found that 100 MB is a good stack size.");
	Dialog.addMessage("___________________________________");
//	Dialog.addMessage("Define appropriate resampling factor.");
//	Dialog.addNumber("resampling :", 1);
//	Dialog.addMessage("Usually 1 in this script, because we are.");
//	Dialog.addMessage("    iterating through decently sized ROIs.");
//	Dialog.addMessage("___________________________________");
	Dialog.addCheckbox("Extract the surface of the whole eye?", false);
	Dialog.addMessage("     This can be helpful to check the results.");
	Dialog.addMessage("     The stack will be scaled down before.");
	Dialog.addMessage("___________________________________");
	Dialog.show();
	
	threshold = Dialog.getNumber();
	desired_size = Dialog.getNumber();
	// resampling = Dialog.getNumber();
	extract_whole_eye_surface = Dialog.getCheckbox();
	
run("Select None");


if(extract_whole_eye_surface == true){
	// first scale whole stack to 100 MB
	o = width*height*slices;
	perc_d = 100 * pow(desired_size/o,1/3);
	
	print("*************************");
	// print("Desired file size of " + d + " reached by reduction to " + perc_d + "%.");
	// print(perc_d);
	run("Scale...", "x="+perc_d+" y="+perc_d+" z="+perc_d+" interpolation=Bicubic average process create title=CV0028_Apis_mellifera_60292_eye2_CLAHE_cr_sc8243");
	
	
	title_current = getTitle();
	run("3D Viewer");
	call("ij3d.ImageJ3DViewer.setCoordinateSystem", "false");
	call("ij3d.ImageJ3DViewer.add", title_current, "White", title_current, threshold, "true", "true", "true", "1", "2");
	
	waitForUser("Save STL...", "Save binary STL file for the whole eye manually.\nName it eye.stl\nClick okay AFTER saving is finished.");
	call("ij3d.ImageJ3DViewer.close");
	selectImage(title_current);
	close(); 
}

selectImage(title_original);
//Dialog.create("Stack size");
//Dialog.addNumber("Desired stack size for each 3D ROI (MB)", 100);
//Dialog.addNumber("Desired stack size", 0);
//Dialog.addString("Unit", 'GB');
//Dialog.show();
//desired_size = Dialog.getNumber;
//d = Dialog.getNumber;
//unit = Dialog.getString;

o = width*height*slices;
d = desired_size * 100*100*100; //MB
// 1,000,000
//39,600,000
unit = "um";

perc_d = 100 * pow(d/o,1/3);

ROI_width = round(perc_d/100 * width);
ROI_height = round(perc_d/100 * height);
ROI_slices = round(perc_d/100 * slices);

//print("ROI_width = "+ROI_width);
//print("ROI_height = "+ROI_height);
//print("ROI_slices = "+ROI_slices);
//print("***************************");

number_of_ROIs_x = Math.ceil(width/ROI_width);
number_of_ROIs_y = Math.ceil(height/ROI_height);
number_of_ROIs_z = Math.ceil(slices/ROI_slices);

number_of_ROIs_xyz = number_of_ROIs_x*number_of_ROIs_y*number_of_ROIs_z;

//print("number_of_ROIs_witdh = "+number_of_ROIs_x);
//print("number_of_ROIs_height = "+number_of_ROIs_y);
//print("number_of_ROIs_slices = "+number_of_ROIs_z);

print("There will be a total of "+number_of_ROIs_xyz+" ROIs to go through.");
print("***************************");


print("************************************");
File.makeDirectory(log_dir_path);
ROI_log_file = "STL_extraction_crop.log";
print("Creating log file ("+ROI_log_file+")...");
open_log_file = File.open(log_dir_path+dir_sep+ROI_log_file);
print(open_log_file, "ROI,x,y,z"); // counter + "," + curr_ROI_start_x + curr_ROI_start_y, curr_slice);
//File.close(open_log_file)

counter=1;
for (w = 0; w < number_of_ROIs_x; w++) {
			curr_ROI_start_x = (w)*ROI_width;
	for (h = 0; h < number_of_ROIs_y; h++) {
			curr_ROI_start_y = (h)*ROI_height;
		for (s = 0; s < number_of_ROIs_z; s++) {
			print("************************************");
			print("ROI # = " + counter);
			curr_slice = (s)*ROI_slices+1;
			print("x = " + curr_ROI_start_x + "px = " + curr_ROI_start_x*px_size + "um.");
			print("x = " + curr_ROI_start_y + "px = " + curr_ROI_start_y*px_size + "um.");
			print("x = " + curr_slice + "px = " + curr_slice*px_size + "um.");
			
			setSlice(curr_slice);
			//getSliceNumber();
			curr_ROI_z_slices = ROI_slices;
			// makeRectangle(x, y, width, height);
			// print();
			selectImage(title_original);
			makeRectangle(curr_ROI_start_x, curr_ROI_start_y, ROI_width+1, ROI_height+1);
			run("Duplicate...", "duplicate range="+curr_slice+"-"+(curr_slice+ROI_slices));
			title_current = getTitle();
			run("3D Viewer");
			call("ij3d.ImageJ3DViewer.setCoordinateSystem", "false");
			call("ij3d.ImageJ3DViewer.add", title_current, "White", title_current, threshold, "true", "true", "true", "1", "2");
			
			waitForUser("Save STL...", "Save binary STL file for the current ROI manually.\nCall it "+counter+".stl\nClick okay AFTER saving is finished.");
			call("ij3d.ImageJ3DViewer.close");
			selectImage(title_current);
			close(); 
			
			//open_log_file = File.open(log_dir_path+dir_sep+ROI_log_file);
			print(open_log_file, counter + "," + (curr_ROI_start_x)*px_size + "," + (curr_ROI_start_y)*px_size + "," + (curr_slice-1)*px_size);
			//File.close(open_log_file)
			
			counter++;
		}
	}
}

File.close(open_log_file)
print("************************************");
selectImage(title_original);
//close(); 
print("All done!");


