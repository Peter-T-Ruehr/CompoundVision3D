bl_info = {
    "name": "CompoundVision3D",
    "blender": (3, 0, 0),
    "category": "View3D",
    "author": "Peter T. Rühr",
    "version": (0, 0, 9023),
    "description": "CompoundVision3D Blender Plugin. Part of the compound eye property extraction workflow published by Rühr, Pande & Blanke: xxx.",
}

import bpy
import csv
import os.path as p
import re

        
# ------------------ SCRIPT 1 ------------------
class SCRIPT_1_OT_Operator(bpy.types.Operator):
    bl_idname = "script.execute_1"
    bl_label = "Step 1: Rescale mesh"
    
    def execute(self, context):
        bpy.ops.transform.resize(value=(0.001, 0.001, 0.001), orient_type='GLOBAL')
        bpy.ops.object.editmode_toggle()
        bpy.ops.mesh.normals_make_consistent(inside=False)
        bpy.ops.mesh.select_all(action='SELECT')
        bpy.ops.mesh.flip_normals()
        bpy.ops.mesh.select_all(action='DESELECT')
        for a in bpy.context.screen.areas:
            if a.type == 'VIEW_3D':
                for s in a.spaces:
                    if s.type == 'VIEW_3D':
                        s.clip_end = 10000
                        s.overlay.show_stats = True
        self.report({'INFO'}, "Script 1 executed!")
        return {'FINISHED'}

# ------------------ SCRIPT 2 ------------------
class SCRIPT_2_OT_Operator(bpy.types.Operator):
    bl_idname = "script.execute_2"
    bl_label = "Step 2: Add modifiers before export"
    
    def execute(self, context):
        bpy.ops.object.modifier_add(type='DECIMATE')
        bpy.context.object.modifiers["Decimate"].ratio = 0.5  # Example ratio
        bpy.ops.object.modifier_add(type='SMOOTH')
        bpy.context.object.modifiers["Smooth"].iterations = 10
        bpy.context.object.modifiers["Smooth"].factor = 0.5
        bpy.ops.object.editmode_toggle()
        bpy.ops.mesh.select_all(action='SELECT')
        bpy.ops.mesh.flip_normals()
        bpy.ops.object.editmode_toggle()
        self.report({'INFO'}, "Script 2 executed!")
        return {'FINISHED'}

# ------------------ SCRIPT 3 ------------------
class SCRIPT_3_OT_Operator(bpy.types.Operator):
    bl_idname = "script.execute_3"
    bl_label = "Run Script 3"
    
    def execute(self, context):
        curr_filename_raw = bpy.path.basename(bpy.context.blend_data.filepath)
        curr_filename = re.sub(".blend$", "", curr_filename_raw)
        curr_filepath = bpy.path.abspath("//")
        candidates_filepath = re.sub("1_pre_STLs.+$", "6_facet_candidates", curr_filepath)
        
        csv_file_path = p.join(candidates_filepath, curr_filename+'_facet_candidates.csv')
        file = csv.reader(open(csv_file_path, newline=''), delimiter=',')
        radius = 0.01
        for idx, row in enumerate(file):
            if idx > 0:
                x = float(row[1])
                y = float(row[2])
                z = float(row[3])
                bpy.ops.object.empty_add(type='SPHERE', radius=radius, align='WORLD', location=(x/1000, y/1000, z/1000), scale=(1, 1, 1))
        self.report({'INFO'}, "Script 3 executed!")
        return {'FINISHED'}

# ------------------ SCRIPT 4 ------------------
class SCRIPT_4_OT_Operator(bpy.types.Operator):
    bl_idname = "script.execute_4"
    bl_label = "Run Script 4"
    
    def execute(self, context):
        curr_filename_raw = bpy.path.basename(bpy.context.blend_data.filepath)
        curr_filename = re.sub(".blend$", "", curr_filename_raw)
        curr_filepath = bpy.path.abspath("//")
        positions_filepath = re.sub("1_pre_STLs.+$", "7_facet_positions", curr_filepath)
        
        coordinate_multiplicator = 1000
        
        output_path = p.join(positions_filepath, curr_filename+'_facet_positions.csv')
        
        selected_objects = bpy.context.selected_objects
        with open(output_path, "w") as output:
            output.write("ID,x,y,z\n")
            for obj in selected_objects:
                string_to_write = (f"{obj.name}", str(obj.location[0]*coordinate_multiplicator), str(obj.location[1]*coordinate_multiplicator), str(obj.location[2]*coordinate_multiplicator), "\n")
                output.write(','.join(string_to_write))
        self.report({'INFO'}, f"Data exported to: {output_path}")
        return {'FINISHED'}

# ------------------ GUI PANEL ------------------
class MYPLUGIN_PT_Panel(bpy.types.Panel):
    bl_label = "CompoundVision3D Panel"
    bl_idname = "MYPLUGIN_PT_Panel"
    bl_space_type = "VIEW_3D"
    bl_region_type = "UI"
    bl_category = "CompoundVision3D"
    
    def draw(self, context):
        layout = self.layout
        layout.label(text="Execute Steps:")
        layout.operator("script.execute_1", text="1: Rescale mesh")
        layout.operator("script.execute_2", text="2: Add modifiers")
        layout.operator("script.execute_3", text="3: Import candidates")
        layout.operator("script.execute_4", text="4: Export positions")

# ------------------ REGISTER ADD-ON ------------------
classes = [
    SCRIPT_1_OT_Operator,
    SCRIPT_2_OT_Operator,
    SCRIPT_3_OT_Operator,
    SCRIPT_4_OT_Operator,
    MYPLUGIN_PT_Panel,
]

def register():
    for cls in classes:
        bpy.utils.register_class(cls)

def unregister():
    for cls in reversed(classes):
        bpy.utils.unregister_class(cls)

if __name__ == "__main__":
    register()
