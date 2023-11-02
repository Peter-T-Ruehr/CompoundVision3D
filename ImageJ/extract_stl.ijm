title = getTitle();
setTool("line");

Stack.getDimensions(width,height,channels,slices,frames);
setSlice(slices/2);

waitForUser("Threshold", "Find appropriate threshold level.");
	Dialog.create("Threshold");
	Dialog.addMessage("Please eappropriate threshold level.");
	Dialog.addNumber("Threshold :", 100);
	Dialog.show();
	
	threshold = Dialog.getNumber();
	
//waitForUser("Resampling factor", "Set resampling factor.");
	Dialog.create("Resampling factor");
	Dialog.addMessage("Please eappropriate resampling factor.");
	Dialog.addNumber("resampling :", 1);
	Dialog.addMessage("Usually 1; between 1 and 2 for larger image stacks.");
	Dialog.show();
	
	resampling = Dialog.getNumber();
	
run("3D Viewer");
call("ij3d.ImageJ3DViewer.setCoordinateSystem", "false");
call("ij3d.ImageJ3DViewer.add", title, "White", title, threshold, "true", "true", "true", resampling, "2");

waitForUser("Save STL...", "Save binary STL file for the eye manually. Click okay AFTER saving is finished.");
call("ij3d.ImageJ3DViewer.close");