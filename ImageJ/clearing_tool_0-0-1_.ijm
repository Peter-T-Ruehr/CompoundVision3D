Stack.getDimensions(width, height, channels, slices, frames);
curr_slice = getSliceNumber();

waitForUser("1) Draw the Region of Interest (ROI)\nwith any selection tool from the menue.\n2) Click 'Ok' AFTERWARDS."); 

Dialog.create("Presettings");
Dialog.addMessage("Clearing tool");
Dialog.addMessage("___________________________________");
Dialog.addCheckbox("Clear inside the ROI", true);
Dialog.addCheckbox("Background color black", true);
Dialog.addMessage("___________________________________");
Dialog.addCheckbox("Clear range", true);
Dialog.addNumber("first slice", 1);
Dialog.addNumber("last slice", curr_slice);
Dialog.show();
cl_inside = Dialog.getCheckbox();
bg_black = Dialog.getCheckbox();
cl_range = Dialog.getCheckbox();
slice_start = Dialog.getNumber();
slice_last = Dialog.getNumber()+1;


if(bg_black == true){
	run("Colors...", "foreground=white background=black selection=yellow");
} else {
	run("Colors...", "foreground=white background=white selection=yellow");
}

run("Set Measurements...", "  area redirect=None decimal=3");
run("Measure");
area_ROI_um = getResult("Area", 0);
run("Clear Results");
if (isOpen("Results")) { 
     selectWindow("Results"); 
     run("Close"); 
}

getSelectionCoordinates(x, y);

if (cl_range == true){
	cl_ROI_range(cl_inside, x, y, slice_start, slice_last);
}
else{
	cl_ROI_all(cl_inside, x, y);
}

msg = "Finished.";


/****************** F U N C T I O N S *************************/

function cl_ROI_all(cl_inside, x, y){
	makeSelection("polygon", x, y);
	setBackgroundColor(0, 0, 0);
	if(cl_inside){
		run("Clear", "stack");
	}
	else {
		run("Clear Outside", "stack");
	}
}

function cl_ROI_range(cl_inside, x, y, slice_start, slice_last){
	for (j=slice_start; j<slice_last; j++) { 
		Stack.setSlice(j); 
		if(cl_inside){
		run("Clear", "slice");
		}
		else {
			run("Clear Outside", "slice");
		}	
	}
}
