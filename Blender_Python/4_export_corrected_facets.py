import bpy
from os import path as p
bpy.context.scene.unit_settings.length_unit = 'CENTIMETERS'

coordinate_multiplicator = 1000

selected_objects = bpy.context.selected_objects
# output_path = p.join('X:/Pub/2023/Krieger_Hartzsch_Krillauge/data/7_corrected_facet_postitions', "KR0001_Krill01fem4x_TIFF_eye1_surface_facet_positions.csv")
# output_path = p.join('X:/Pub/2019/Ruehr_compound_vision/compound_vision_3D_paper/data/7_fine_clusters_corrected', "CV0003_Zephronia_viridisoma_Micro_CT_8bit_eye2_fine_clusters.csv")
output_path = p.join('C:/Users/pruehr.EVOLUTION/Documents/tmp', "CV0005_Apis_mellifera_60185_eye1_facet_positions.csv")



with open(output_path, "w") as output:
    output.write("ID,x,y,z\n")

    for obj in selected_objects:
        # print(obj.location[0:2])
        string_to_write = (f"{obj.name}", str(obj.location[0]*coordinate_multiplicator), str(obj.location[1]*coordinate_multiplicator), str(obj.location[2]*coordinate_multiplicator), "\n")
        output.write(','.join(string_to_write))

print(f"Data exported to: {output_path}")
print(f"Coordinates multiplied by {coordinate_multiplicator}.")
print(f"All done!")
