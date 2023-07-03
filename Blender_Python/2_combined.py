import bpy

# apply scale and modifiers
bpy.ops.object.modifier_add(type='DECIMATE')
bpy.context.object.modifiers["Decimate"].ratio = 0.25
bpy.ops.object.modifier_add(type='SMOOTH')
bpy.context.object.modifiers["Smooth"].iterations = 3

# flip normales
bpy.ops.object.editmode_toggle()
bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.flip_normals()
bpy.ops.object.editmode_toggle()
