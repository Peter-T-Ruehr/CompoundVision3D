import bpy
import csv
bpy.context.scene.unit_settings.length_unit = 'CENTIMETERS'
import re


# steps:
#     1 = import STL files of eye parts
#     2 = translate eye part objects
#     3 = fuse objects and remove close vertices
#     "all" = run all steps.
#     0 (or any other number than 1-3 or any string except "all") = prints error message

step = 0

curr_filename_raw = bpy.path.basename(bpy.context.blend_data.filepath)
curr_filename = re.sub(".blend$", "", curr_filename_raw)
curr_filepath = bpy.path.abspath("//")


print(curr_filename)
print("in:")
print(curr_filepath)


if step == 1:
    bpy.ops.transform.resize(value=(0.001, 0.001, 0.001), orient_type='GLOBAL', orient_matrix=((1, 0, 0), (0, 1, 0), (0, 0, 1)), orient_matrix_type='GLOBAL', mirror=True, use_proportional_edit=False, proportional_edit_falloff='SMOOTH', proportional_size=1, use_proportional_connected=False, use_proportional_projected=False)
    # W + W: Lasso selection
    # bpy.ops.mesh.select_more()
    for a in bpy.context.screen.areas:
        if a.type == 'VIEW_3D':
            for s in a.spaces:
                if s.type == 'VIEW_3D':
                    s.clip_end = 10000
                    s.overlay.show_stats = True

if step == 2:           
    # Path to your CSV file
    csv_file_path = '//datastorage/users/pruehr/Pub/2019/Ruehr_compound_vision/_bkp/multiple_STL_extraction/STL_extraction_log/STL_extraction_crop.log'

    # Read the CSV file
    with open(csv_file_path, newline='') as csvfile:
        csv_reader = csv.reader(csvfile)
        
        # Skip header (if any)
        next(csv_reader, None)
        
        # Loop through the rows of the CSV file
        for i, row in enumerate(csv_reader):
            k = i+1
            # Assume CSV has three columns: X, Y, Z coordinates
            ID, x, y, z = map(float, row)  # Convert the strings to floats
            
            # Get the object by index (or name, depending on how you access objects)
            obj_name = f"{k}"  # Assuming object names are like "Object_0", "Object_1", etc.
            print(obj_name)
            
            print(x)
            print(y)
            print(z)
            
            x=x*0.001
            y=y*0.001
            z=z*0.001
            
            obj = bpy.data.objects.get(obj_name)
            
            if obj:
                # Translate the object by setting its location
                obj.location = (x, y, z)
                
#                obj.location.x += x
#                obj.location.y += y
#                obj.location.z += z
                
            else:
                print(f"Object {obj_name} not found")

if step == 3:
    bpy.ops.object.select_all(action='SELECT')
    bpy.ops.object.join()
    bpy.ops.object.editmode_toggle()
    bpy.ops.mesh.remove_doubles(threshold=0.02, use_unselected=True)
    bpy.ops.object.editmode_toggle()

    
else:
    print("No usable step number [1-3]")
