// Recursively lists the files in a user-specified directory.
// Open a file on the list by double clicking on it.

dir = getDirectory("Choose a Directory ");
count = 1;
listFiles(dir); 

function listFiles(dir) {
 list = getFileList(dir);
 for (i=0; i<list.length; i++) {
    if (endsWith(list[i], "/"))
       listFiles(""+dir+list[i]);
   else 
		if(endsWith(dir+list[i], "tif")){
			count = count++;
			print((count) + ": " + dir+list[i]);
			open(dir+list[i]);
			nrrd_file = replace(dir+list[i], "\\.tif", "\\.nrrd");
			print((count) + ": " + nrrd_file);
			run("Nrrd ... ", "nrrd=[nrrd_file]");
			while (nImages>0) { 
	          selectImage(nImages); 
	          close(); 
			}
		}
 }
}
