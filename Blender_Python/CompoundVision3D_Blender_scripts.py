import bpy, csv, math
from math import pi
from os import path as p
bpy.context.scene.unit_settings.length_unit = 'CENTIMETERS'
import re

# steps:
#     1 = import STL file of current eye to manually extract its surface
#     2 = add modifiers before manual export of the extracted surface of current eye as ASCII STL
#     3 = import facet position results of R algorithm
#     4 = export facet positions after manual check and corrections; define the path you want to export in next line.
#     0 (or any other number than 1-4 or any string) = prints error message

step = 0



# Step 3 Settings:
radius = 0.0025 # 0.025 0.0025
segments = 16 # 4 16
ring_count = 8 # 3 8 


# Step 4 Settings
export_to = 'data'   
                        # Choose 'data' to follow the folder convention of the CompoundVision3D workflow. This will save the data in ./data/7_facet_positions/. My default.
                        # If choosing 'current', Blender will try to export to the directory of the currently open Blender file.
                        # Choose 'documents' to save to 'documents/tmp/'. Sometimes, in case you're running the files on a mapped server, it may not be accessible for wirting, so the local documents folder is my go-to.




curr_filename_raw = bpy.path.basename(bpy.context.blend_data.filepath)
curr_filename = re.sub(".blend$", "", curr_filename_raw)
curr_filepath = bpy.path.abspath("//")


print(curr_filename)
print("in:")
print(curr_filepath)


if step == 1:
    bpy.ops.transform.resize(value=(0.001, 0.001, 0.001), orient_type='GLOBAL', orient_matrix=((1, 0, 0), (0, 1, 0), (0, 0, 1)), orient_matrix_type='GLOBAL', mirror=True, use_proportional_edit=False, proportional_edit_falloff='SMOOTH', proportional_size=1, use_proportional_connected=False, use_proportional_projected=False)
    bpy.ops.object.editmode_toggle()
    bpy.ops.mesh.normals_make_consistent(inside=False)
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.flip_normals()
    bpy.ops.mesh.select_all(action='DESELECT')
    # W + W: Lasso selection
    # bpy.ops.mesh.select_more()
    for a in bpy.context.screen.areas:
        if a.type == 'VIEW_3D':
            for s in a.spaces:
                if s.type == 'VIEW_3D':
                    s.clip_end = 10000
                    s.overlay.show_stats = True



elif step == 2:
    import bpy
    # bpy.context.scene.unit_settings.length_unit = 'CENTIMETERS'

    # apply scale and modifiers
    bpy.ops.object.modifier_add(type='DECIMATE')
    bpy.context.object.modifiers["Decimate"].ratio = 0.25
    bpy.ops.object.modifier_add(type='SMOOTH')
    bpy.context.object.modifiers["Smooth"].iterations = 3
    bpy.context.object.modifiers["Smooth"].factor = 0.5


    # flip normales
    bpy.ops.object.editmode_toggle()
    bpy.ops.mesh.select_all(action='SELECT')
    bpy.ops.mesh.flip_normals()
    bpy.ops.object.editmode_toggle()

    # toggle face orientation (normal) view
    #         - eye outside surface should be blue and inside red


elif step == 3:
    camera_type = 'PERSP' # 'ORTHO' 'PERSP'

    def hex2col(hex, normalize=False, precision=None):
        col = []
        it = iter(hex)
        for char in it:
            col.append(int(char + it.__next__(), 16))
            
                    
        if normalize:
            col = map(lambda x: x / 255, col)
            
            if precision is not None:
                col = map(lambda x: round(x, precision), col)
                    
        return list(col)


    ## select all
    #bpy.ops.object.select_all(action='SELECT')

    ## remove all
    #bpy.ops.object.delete(use_global=False, confirm=False)

    file = csv.reader(open(p.join('X:/Pub/2019/Ruehr_compound_vision/compound_vision_3D_paper/data/6_fine_clusters/', curr_filename + '_fine_clusters.csv'), newline=''), delimiter=',') # CV0020_Grylloblatta 16 bit_eye2_fine_clusters
    # file = csv.reader(open(p.join('X:/Pub/2019/Ruehr_compound_vision/compound_vision_3D_paper/data/6_fine_clusters/', 'CV0011_Tricholepidion_gertschi_Blanke_4025_eye2_fine_clusters.csv'), newline=''), delimiter=',') # CV0020_Grylloblatta 16 bit_eye2_fine_clusters

    print(file)

    #bpy.ops.material.new()
    #bpy.data.materials["Material"].node_tree.nodes["Principled BSDF"].inputs[0].default_value = (0, 0, 0, 1)

    print("Importing ommatidia coordinate data - this might take a bit...")
    for idx, row in enumerate(file):
    #    print(idx, row)
        if idx > 0:
            
            x = float(row[0])
            y = float(row[1])
            z = float(row[2])

            bpy.ops.mesh.primitive_uv_sphere_add( location = ( x/1000, y/1000, z/1000 ),
                segments=segments, ring_count=ring_count, radius=radius)

            ob = bpy.context.object
            ob.name = row[3]
            
            me = ob.data
            
            curr_material = 'Material_'+str(idx)
            curr_col = "67032F" 
            curr_RGB_ = hex2col(curr_col)
            curr_RGB = [x / 255 for x in curr_RGB_]
            # print(curr_material, ': Hex#', curr_col,  ' --> RGB ', curr_RGB)
            curr_RGB.append(1)
            # print(curr_RGB)
            
            mat = bpy.data.materials.new(name=curr_material)
            mat.diffuse_color = curr_RGB
            me.materials.append(mat)
            
            if idx % 10 == 0:
                print(idx)

    print("All done")


elif step == 4:
    # import bpy
    # from os import path as p
    # bpy.context.scene.unit_settings.length_unit = 'CENTIMETERS'
    
        
    def get_documents_path():
        # Cross-platform way to get the user's home directory
        home_dir = p.expanduser("~")
        
        # Construct the path to the Documents directory based on the platform
        if os.name == 'nt':  # For Windows
            documents_dir = p.join(home_dir, 'Documents')
        elif os.name == 'posix':  # For Unix/Linux/MacOS
            documents_dir = p.join(home_dir, 'Documents')
        
        return documents_dir


    coordinate_multiplicator = 1000

    if export_to == 'data': # documents current other
        # Get the parent directories of the current file
        stl_dir = p.dirname(curr_filepath)
        eye_dir = p.dirname(stl_dir)
        AV_dir = p.dirname(eye_dir)
        pre_STL_dir = p.dirname(AV_dir)
        data_dir = p.dirname(pre_STL_dir)
        
        output_path = p.join(data_dir, '7_facet_positions')

    elif export_to == 'documents': # documents current other
        documents_path = get_documents_path()
        output_path = p.join(documents_path,'tmp')
        print("Saving files to:", output_path)
        
    elif export_to == 'current': # documents current other
        output_path = curr_filepath
        
    selected_objects = bpy.context.selected_objects
    # output_path = p.join('X:/Pub/2023/Krieger_Hartzsch_Krillauge/data/7_corrected_facet_postitions', "KR0001_Krill01fem4x_TIFF_eye1_surface_facet_positions.csv")
    # output_path = p.join('X:/Pub/2019/Ruehr_compound_vision/compound_vision_3D_paper/data/7_fine_clusters_corrected', "CV0003_Zephronia_viridisoma_Micro_CT_8bit_eye2_fine_clusters.csv")
    output_path = p.join(output_path, curr_filename + "_facet_positions.csv")

    with open(output_path, "w") as output:
        output.write("ID,x,y,z\n")

        for obj in selected_objects:
            # print(obj.location[0:2])
            string_to_write = (f"{obj.name}", str(obj.location[0]*coordinate_multiplicator), str(obj.location[1]*coordinate_multiplicator), str(obj.location[2]*coordinate_multiplicator), "\n")
            output.write(','.join(string_to_write))

    print(f"Data exported to: {output_path}")
    print(f"Coordinates multiplied by {coordinate_multiplicator}.")
    print(f"All done!")

else:
    print("No usable step number [1-4]")