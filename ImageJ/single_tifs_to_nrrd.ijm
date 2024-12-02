script_version = "0.0.9001";

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

// when everything is finished, remove all tif and checkpoint files:
// open PowerShell
// run cd X:\Pub\2021\_Ruehr_AntVision\data\tmp
// run del /S *.tif
// run del /S *.ckpt