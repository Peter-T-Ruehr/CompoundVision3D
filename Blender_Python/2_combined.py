import bpy

# apply scale and modifiers
bpy.ops.object.modifier_add(type='DECIMATE')
bpy.context.object.modifiers["Decimate"].ratio = 0.25
bpy.ops.object.modifier_add(type='SMOOTH')
bpy.context.object.modifiers["Smooth"].iterations = 3
bpy.context.object.modifiers["Smooth"].factor = 0.2


# flip normales
bpy.ops.object.editmode_toggle()
bpy.ops.mesh.select_all(action='SELECT')
bpy.ops.mesh.flip_normals()
bpy.ops.object.editmode_toggle()

## check normals - eye outside surface should be blue and inside red
# bpy.context.space_data.overlay.show_face_orientation = True