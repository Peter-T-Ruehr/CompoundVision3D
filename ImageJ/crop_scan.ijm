script_version = "0.9.14";

// data at \\blanke-nas-1\DATA\RAWDATA\Hexapoda\7_Holometabola\Hymenoptera

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
while (nImages>0) { 
          selectImage(nImages); 
          close(); 
}

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
specimen_name = parent_dir_name;

log_dir_path = parent_dir_path+dir_sep+'crop_log';
while(File.exists(log_dir_path)){
	waitForUser("Please delete or rename the ROI folder ("+log_dir_path+")!");
}

if(File.exists(parent_dir_path+dir_sep+"DICOMDIR")){
	print("Trying to open: "+parent_dir_path+"DICOMDIR...");
	run("Bio-Formats", "open=["+parent_dir_path+"DICOMDIR] color_mode=Default rois_import=[ROI manager] view=Hyperstack stack_order=XYCZT"); // display_metadata use_virtual_stack
} 
else {
	print("Trying to open dcm files in "+parent_dir_path+dir_sep+"...");
	run("Image Sequence...", "open="+parent_dir_path+dir_sep+"*.dcm file=.dcm sort");
}

Stack.getDimensions(width,height,channels,slices,frames);
setSlice(slices/2);
makeRectangle(width/4, height/4, width/2, height/2);
resetMinAndMax();
	
getPixelSize(unit, px_size, ph, pd);

rename("original");
selectWindow("original");
// make sure pixel size is identical in all 3 dimensions
run("Properties...", "channels=1 slices=slices frames=1 pixel_width=px_size pixel_height=px_size voxel_depth=px_size");

// define rectangle for BC
setTool("rectangle");
waitForUser("Define rectangle and get grey values for contrast enhancement.");
run("Brightness/Contrast...");
run("Select None");
Dialog.create("Settings");
Dialog.addMessage("___________________________________");
	Dialog.addNumber("Minimum value: ", 10000, 0, 5, " of histogram");
	Dialog.addNumber("Maximum value: ", 46000, 0, 5, " of histogram");
	Dialog.show();
	
	min = Dialog.getNumber();
	max = Dialog.getNumber();
	

//getRawStatistics(count, mean, min, max, std);
run("Select None");
run("Min...", "value=min stack");
run("Max...", "value=max stack");

// define rectangle for crop
setTool("rectangle");
waitForUser("Define rectangle for head ROI.");
getSelectionCoordinates(x_crop, y_crop);
run("Select None");

// define rectangle for crop
setTool("rectangle");
waitForUser("Define rectangle for eye1 ROI.");
getSelectionCoordinates(x_eye1, y_eye1);

waitForUser("1) Check stack for first and last image number in z direction of eye1 ROI\n2) AFTERWARDS, click 'Ok'."); 
	curr_slice = getSliceNumber();
	Dialog.create("Welcome");
	Dialog.addMessage("Please enter number of first and last image.");
	Dialog.addMessage("___________________________________");
	Dialog.addNumber("First image:", 1);
	Dialog.addNumber("Last image:", curr_slice);
	Dialog.show();
	
	first_image_eye1 = Dialog.getNumber();
	last_image_eye1 = Dialog.getNumber();

run("Select None");

// define rectangle for crop
setTool("rectangle");
waitForUser("Define rectangle for eye2 ROI.");
getSelectionCoordinates(x_eye2, y_eye2);

waitForUser("1) Check stack for first and last image number in z direction of eye2 ROI\n2) AFTERWARDS, click 'Ok'."); 
	curr_slice = getSliceNumber();
	Dialog.create("Welcome");
	Dialog.addMessage("Please enter number of first and last image.");
	Dialog.addMessage("___________________________________");
	Dialog.addNumber("First image:", 1);
	Dialog.addNumber("Last image:", curr_slice);
	Dialog.show();
	
	first_image_eye2 = Dialog.getNumber();
	last_image_eye2 = Dialog.getNumber();


// check if scaling is necessary for head-stack checkpoint
Dialog.create("Scaling settings");
	Dialog.addMessage("___________________________________");
	Dialog.addNumber("Scale to [MB]: ", 120);
	Dialog.addMessage("___________________________________");
	Dialog.addMessage("PTR, Jan. 2022");
	Dialog.addMessage("Bonn Univ., Bonn, Germany");
	Dialog.show();
	d_size = Dialog.getNumber()/1024;  //MB/1024=GB
	
run("Select None");
run("8-bit");

// crop head
selectWindow("original");
run("Duplicate...", "duplicate");
makeRectangle(x_crop[0], y_crop[0], x_crop[2]-x_crop[0], y_crop[2]-y_crop[0]);
run("Crop");
title_head = getTitle();

//Create target directory to save tiffs in
print("Creating target directory...");
ROI_name = "head";
ROI_path = parent_dir_path+dir_sep+specimen_name+"_"+ROI_name;
File.makeDirectory(ROI_path);
// sace full-sized head stack
saveAs("Tiff", ROI_path+dir_sep+specimen_name+"_"+ROI_name+".tif");

// calculate if scaling is necessary later
Stack.getDimensions(width_orig, height_orig, channels, slices, frames);
o_size = width_orig*height_orig*slices/(1024*1024*1024);
print("Target directory loaded has a size of ~"+o_size+" GB.");
d = pow(d_size/o_size,1/3);
perc_d = round(100 * d);
d = perc_d/100;

if(perc_d<100){
	print("Scaling head stack to "+perc_d+"% to reach stack size of ~"+d_size+" GB...");
	run("Scale...", "x="+d+" y="+d+" z="+d+" interpolation=Bicubic average process create");
	print("New px size = "+px_size+" um.");
	saveAs("Tiff", ROI_path+dir_sep+specimen_name+"_"+ROI_name+"_sc"+perc_d+".tif");
}
getPixelSize(unit_, px_size_sc, ph, pd);

// save head as Checkpoint
checkpoint_file = File.open(ROI_path+dir_sep+specimen_name+"_"+ROI_name+".ckpt");
print(checkpoint_file, "Version 5");
print(checkpoint_file, "Stratovan Checkpoint (TM)");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Information]");
print(checkpoint_file, "Name: "+specimen_name+"_"+ROI_name+", .ckpt");
print(checkpoint_file, specimen_name+"_"+ROI_name);
print(checkpoint_file, "Birthdate: ");
print(checkpoint_file, "Sex: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Study]");
print(checkpoint_file, "StudyInstanceUID: ");
print(checkpoint_file, "StudyID: ");
print(checkpoint_file, "StudyDate: ");
print(checkpoint_file, "StudyTime: ");
print(checkpoint_file, "StudyDescription: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Series]");
print(checkpoint_file, "SeriesInstanceUID: ");
print(checkpoint_file, "SeriesNumber: ");
print(checkpoint_file, "SeriesDate: ");
print(checkpoint_file, "SeriesTime: ");
print(checkpoint_file, "SeriesModality: ");
print(checkpoint_file, "SeriesProtocol: ");
print(checkpoint_file, "SeriesPart: ");
print(checkpoint_file, "SeriesDescription: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen File(s)]");
print(checkpoint_file, "NumberOfFolders: 1");
print(checkpoint_file, "Folder: "+ROI_path);
print(checkpoint_file, "");
print(checkpoint_file, "[Surface Information]");
print(checkpoint_file, "NumberOfSurfaces: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Templates]");
print(checkpoint_file, "NumberOfTemplates: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Landmarks]");
print(checkpoint_file, "NumberOfPoints: 0");
print(checkpoint_file, "Units: um");
print(checkpoint_file, "");
print(checkpoint_file, "[SinglePoints]");
print(checkpoint_file, "NumberOfSinglePoints: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Curves]");
print(checkpoint_file, "NumberOfCurves: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Patches]");
print(checkpoint_file, "NumberOfPatches: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Joints]");
print(checkpoint_file, "NumberOfJoints: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Lengths]");
print(checkpoint_file, "NumberOfLengths: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Lines]");
print(checkpoint_file, "NumberOfLines: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Angles]");
print(checkpoint_file, "NumberOfAngles: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Planes]");
print(checkpoint_file, "NumberOfPlanes: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Image Stack]");
print(checkpoint_file, "Units: um");
if(perc_d<100){
	print(checkpoint_file, "Spacing: "+px_size_sc+" "+px_size_sc+" "+px_size_sc+" ");
}
else{
	print(checkpoint_file, "Spacing: "+px_size+" "+px_size+" "+px_size+" ");
}
print(checkpoint_file, "NumberOfFiles: 1");
if(perc_d<100){
	print(checkpoint_file, "Files: \""+specimen_name+"_"+ROI_name+"_sc"+perc_d+".tif\"");
}
else{
	print(checkpoint_file, "Files: \""+specimen_name+"_"+ROI_name+".tif\"");
}
print(checkpoint_file, "");
print(checkpoint_file, "[Contrast and Brightness]");
print(checkpoint_file, "Width: 82");
print(checkpoint_file, "Level: -19");
print(checkpoint_file, "");
print(checkpoint_file, "[Landmark Size]");
print(checkpoint_file, "Size: 2");
File.close(checkpoint_file);
print("Saved checkpoint file as "+ROI_path+dir_sep+specimen_name+"_"+ROI_name+".ckpt");

// crop eye 1
selectWindow("original");
run("Duplicate...", "duplicate");
run("Make Substack...", " slices="+first_image_eye1+"-"+last_image_eye1);
makeRectangle(x_eye1[0], y_eye1[0], x_eye1[2]-x_eye1[0], y_eye1[2]-y_eye1[0]);
run("Crop");
title_eye1 = getTitle();

//Create target directory to save tiffs in
print("Creating target directory...");
ROI_name = "eye1";
ROI_path = parent_dir_path+dir_sep+specimen_name+"_"+ROI_name;
File.makeDirectory(ROI_path);
//run("Image Sequence... ", "dir=" + ROI_path + " format=TIFF name=" + specimen_name+"_"+ROI_name+"_");
saveAs("Tiff", ROI_path+dir_sep+specimen_name+"_"+ROI_name+".tif");

// save eye1 as Checkpoint
checkpoint_file = File.open(ROI_path+dir_sep+specimen_name+"_"+ROI_name+".ckpt");
print(checkpoint_file, "Version 5");
print(checkpoint_file, "Stratovan Checkpoint (TM)");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Information]");
print(checkpoint_file, "Name: "+specimen_name+"_"+ROI_name+", .ckpt");
print(checkpoint_file, specimen_name+"_"+ROI_name);
print(checkpoint_file, "Birthdate: ");
print(checkpoint_file, "Sex: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Study]");
print(checkpoint_file, "StudyInstanceUID: ");
print(checkpoint_file, "StudyID: ");
print(checkpoint_file, "StudyDate: ");
print(checkpoint_file, "StudyTime: ");
print(checkpoint_file, "StudyDescription: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Series]");
print(checkpoint_file, "SeriesInstanceUID: ");
print(checkpoint_file, "SeriesNumber: ");
print(checkpoint_file, "SeriesDate: ");
print(checkpoint_file, "SeriesTime: ");
print(checkpoint_file, "SeriesModality: ");
print(checkpoint_file, "SeriesProtocol: ");
print(checkpoint_file, "SeriesPart: ");
print(checkpoint_file, "SeriesDescription: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen File(s)]");
print(checkpoint_file, "NumberOfFolders: 1");
print(checkpoint_file, "Folder: "+ROI_path);
print(checkpoint_file, "");
print(checkpoint_file, "[Surface Information]");
print(checkpoint_file, "NumberOfSurfaces: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Templates]");
print(checkpoint_file, "NumberOfTemplates: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Landmarks]");
print(checkpoint_file, "NumberOfPoints: 0");
print(checkpoint_file, "Units: um");
print(checkpoint_file, "");
print(checkpoint_file, "[SinglePoints]");
print(checkpoint_file, "NumberOfSinglePoints: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Curves]");
print(checkpoint_file, "NumberOfCurves: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Patches]");
print(checkpoint_file, "NumberOfPatches: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Joints]");
print(checkpoint_file, "NumberOfJoints: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Lengths]");
print(checkpoint_file, "NumberOfLengths: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Lines]");
print(checkpoint_file, "NumberOfLines: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Angles]");
print(checkpoint_file, "NumberOfAngles: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Planes]");
print(checkpoint_file, "NumberOfPlanes: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Image Stack]");
print(checkpoint_file, "Units: um");
print(checkpoint_file, "Spacing: "+px_size+" "+px_size+" "+px_size+" ");
print(checkpoint_file, "NumberOfFiles: 1");
print(checkpoint_file, "Files: \""+specimen_name+"_"+ROI_name+".tif\"");
print(checkpoint_file, "");
print(checkpoint_file, "[Contrast and Brightness]");
print(checkpoint_file, "Width: 82");
print(checkpoint_file, "Level: -19");
print(checkpoint_file, "");
print(checkpoint_file, "[Landmark Size]");
print(checkpoint_file, "Size: 2");
File.close(checkpoint_file);
print("Saved checkpoint file as "+ROI_path+dir_sep+specimen_name+"_"+ROI_name+".ckpt");

// save STL
STL_path = parent_dir_path+dir_sep+specimen_name+"_"+ROI_name+dir_sep+"stl";
File.makeDirectory(STL_path);
title = getTitle();

Stack.getDimensions(width_eye1,height_eye1,channels_eye1,slices_eye1,frames_eye1);
setSlice(slices_eye1/2);

waitForUser("Threshold", "Find appropriate threshold level.");
	Dialog.create("Threshold");
	Dialog.addMessage("Please eappropriate threshold level.");
	Dialog.addNumber("Threshold :", 100);
	Dialog.show();
	
	threshold_eye1 = Dialog.getNumber();

run("3D Viewer");
call("ij3d.ImageJ3DViewer.setCoordinateSystem", "false");
call("ij3d.ImageJ3DViewer.add", title, "White", title, threshold_eye1, "true", "true", "true", "1", "2");

waitForUser("Save STL...", "Save ASCII STL file manually. Click okay AFTER saving is finished.");
call("ij3d.ImageJ3DViewer.close");

// crop eye 2
selectWindow("original");
run("Duplicate...", "duplicate");
run("Make Substack...", " slices="+first_image_eye2+"-"+last_image_eye2);
makeRectangle(x_eye2[0], y_eye2[0], x_eye2[2]-x_eye2[0], y_eye2[2]-y_eye2[0]);
run("Crop");
title_eye2 = getTitle();

//Create target directory to save tiffs in
print("Creating target directory...");
ROI_name = "eye2";
ROI_path = parent_dir_path+dir_sep+specimen_name+"_"+ROI_name;
File.makeDirectory(ROI_path);
//run("Image Sequence... ", "dir=" + ROI_path + " format=TIFF name=" + specimen_name+"_"+ROI_name+"_");
saveAs("Tiff", ROI_path+dir_sep+specimen_name+"_"+ROI_name+".tif");

// save eye2 as Checkpoint
checkpoint_file = File.open(ROI_path+dir_sep+specimen_name+"_"+ROI_name+".ckpt");
print(checkpoint_file, "Version 5");
print(checkpoint_file, "Stratovan Checkpoint (TM)");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Information]");
print(checkpoint_file, "Name: "+specimen_name+"_"+ROI_name+", .ckpt");
print(checkpoint_file, specimen_name+"_"+ROI_name);
print(checkpoint_file, "Birthdate: ");
print(checkpoint_file, "Sex: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Study]");
print(checkpoint_file, "StudyInstanceUID: ");
print(checkpoint_file, "StudyID: ");
print(checkpoint_file, "StudyDate: ");
print(checkpoint_file, "StudyTime: ");
print(checkpoint_file, "StudyDescription: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen Series]");
print(checkpoint_file, "SeriesInstanceUID: ");
print(checkpoint_file, "SeriesNumber: ");
print(checkpoint_file, "SeriesDate: ");
print(checkpoint_file, "SeriesTime: ");
print(checkpoint_file, "SeriesModality: ");
print(checkpoint_file, "SeriesProtocol: ");
print(checkpoint_file, "SeriesPart: ");
print(checkpoint_file, "SeriesDescription: ");
print(checkpoint_file, "");
print(checkpoint_file, "[Specimen File(s)]");
print(checkpoint_file, "NumberOfFolders: 1");
print(checkpoint_file, "Folder: "+ROI_path);
print(checkpoint_file, "");
print(checkpoint_file, "[Surface Information]");
print(checkpoint_file, "NumberOfSurfaces: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Templates]");
print(checkpoint_file, "NumberOfTemplates: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Landmarks]");
print(checkpoint_file, "NumberOfPoints: 0");
print(checkpoint_file, "Units: um");
print(checkpoint_file, "");
print(checkpoint_file, "[SinglePoints]");
print(checkpoint_file, "NumberOfSinglePoints: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Curves]");
print(checkpoint_file, "NumberOfCurves: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Patches]");
print(checkpoint_file, "NumberOfPatches: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Joints]");
print(checkpoint_file, "NumberOfJoints: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Lengths]");
print(checkpoint_file, "NumberOfLengths: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Lines]");
print(checkpoint_file, "NumberOfLines: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Angles]");
print(checkpoint_file, "NumberOfAngles: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Planes]");
print(checkpoint_file, "NumberOfPlanes: 0");
print(checkpoint_file, "");
print(checkpoint_file, "[Image Stack]");
print(checkpoint_file, "Units: um");
print(checkpoint_file, "Spacing: "+px_size+" "+px_size+" "+px_size+" ");
print(checkpoint_file, "NumberOfFiles: 1");
print(checkpoint_file, "Files: \""+specimen_name+"_"+ROI_name+".tif\"");
print(checkpoint_file, "");
print(checkpoint_file, "[Contrast and Brightness]");
print(checkpoint_file, "Width: 82");
print(checkpoint_file, "Level: -19");
print(checkpoint_file, "");
print(checkpoint_file, "[Landmark Size]");
print(checkpoint_file, "Size: 2");
File.close(checkpoint_file);
print("Saved checkpoint file as "+ROI_path+dir_sep+specimen_name+"_"+ROI_name+".ckpt");

// save STL
STL_path = parent_dir_path+dir_sep+specimen_name+"_"+ROI_name+dir_sep+"stl";
File.makeDirectory(STL_path);
title = getTitle();

Stack.getDimensions(width_eye2,height_eye2,channels_eye2,slices_eye2,frames_eye2);
setSlice(slices_eye2/2);

waitForUser("Threshold", "Find appropriate threshold level.");
	Dialog.create("Threshold");
	Dialog.addMessage("Please eappropriate threshold level.");
	Dialog.addNumber("Threshold :", threshold_eye1);
	Dialog.show();
	
	threshold_eye2 = Dialog.getNumber();

run("3D Viewer");
call("ij3d.ImageJ3DViewer.setCoordinateSystem", "false");
call("ij3d.ImageJ3DViewer.add", title, "White", title, threshold_eye2, "true", "true", "true", "1", "2");

waitForUser("Save STL...", "Save ASCII STL file manually. Click okay AFTER saving is finished.");
call("ij3d.ImageJ3DViewer.close");

print("************************************");
File.makeDirectory(log_dir_path);
ROI_log_file = specimen_name+"_crop.log";
print("Creating log file ("+ROI_log_file+")...");
open_log_file = File.open(log_dir_path+dir_sep+ROI_log_file);
print(open_log_file, "script_version = " + script_version);
print(open_log_file, "px_size = " + px_size + " " + unit);
print(open_log_file, "hist_min = " + min);
print(open_log_file, "hist_max = " + max);
print(open_log_file, "ROI_head = makeRectangle("+x_crop[0]+", "+y_crop[0]+", "+x_crop[1]-x_crop[0]+", "+y_crop[2]-y_crop[0]+");");
print(open_log_file, "ROI_eye1 = makeRectangle("+x_eye1[0]+", "+y_eye1[0]+", "+x_eye1[1]-x_eye1[0]+", "+y_eye1[2]-y_eye1[0]+");");
print(open_log_file, "z_first_eye1 = " + first_image_eye1);
print(open_log_file, "z_last_eye1 = " + last_image_eye1);
print(open_log_file, "threshold_eye1 = " + threshold_eye1);
print(open_log_file, "ROI_eye2 = makeRectangle("+x_eye2[0]+", "+y_eye2[0]+", "+x_eye2[1]-x_eye2[0]+", "+y_eye2[2]-y_eye2[0]+");");
print(open_log_file, "z_first_eye2 = " + first_image_eye2);
print(open_log_file, "z_last_eye2 = " + last_image_eye2);
print(open_log_file, "threshold_eye2 = " + threshold_eye2);
print(open_log_file, "scaling_head = " + d);
print(open_log_file, "px_size_head = " + px_size_sc + " " + unit);
File.close(open_log_file)
print("************************************");

while (nImages>0) { 
          selectImage(nImages); 
          close(); 
}