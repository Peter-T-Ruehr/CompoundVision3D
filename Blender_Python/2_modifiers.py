import bpy
bpy.ops.object.modifier_add(type='DECIMATE')
bpy.context.object.modifiers["Decimate"].ratio = 0.25
bpy.ops.object.modifier_add(type='SMOOTH')
bpy.context.object.modifiers["Smooth"].iterations = 3
