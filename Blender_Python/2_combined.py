import bpy
bpy.context.scene.unit_settings.length_unit = 'CENTIMETERS'

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