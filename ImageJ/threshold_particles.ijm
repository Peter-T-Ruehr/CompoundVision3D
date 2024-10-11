setThreshold(60, 255);
setOption("BlackBackground", false);
run("Convert to Mask", "background=Dark create");
run("Analyze Particles...", "size=1000-Infinity show=Masks display stack");
run("Grays");