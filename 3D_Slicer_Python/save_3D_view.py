# Get the current layout manager
layoutManager = slicer.app.layoutManager()

# Access the 3D view (assuming you are using the first 3D view)
threeDWidget = layoutManager.threeDWidget(0)
threeDView = threeDWidget.threeDView()

# Find all fiducial nodes and hide them
fiducialNodes = slicer.util.getNodesByClass('vtkMRMLMarkupsFiducialNode')
for node in fiducialNodes:
    node.SetDisplayVisibility(False)

# Get the 3D view's MRML node
viewNode = threeDView.mrmlViewNode()

# hide bounding box and axis labels
viewNode.SetBoxVisible(False)
viewNode.SetAxisLabelsVisible(False)

# Set the view to the X-axis (Right view)
#threeDView.lookFromAxis(ctk.ctkAxesWidget.Right)

# pitch (tilting up and down), roll (rotating around the viewing axis), and yaw (turning left and right)
# pitch = 0
# yaw = 180
# roll = 90

# Set the rotation increment for pitch, roll, and yaw (in degrees)
# threeDView.setPitchRollYawIncrement(yaw)

# yaw (turn left)
# threeDView.yaw()

# pitch (tilt up)
# threeDView.pitch()

# rotation increment for pitch, roll, and yaw (in degrees)
# threeDView.setPitchRollYawIncrement(roll)

# Rotate the view by 10 degrees roll (rotate clockwise)
# threeDView.roll()

# Render the scene
slicer.app.processEvents()

# Set background to black (required for transparent background)
view = slicer.app.layoutManager().threeDWidget(0).threeDView()
view.mrmlViewNode().SetBackgroundColor(0,0,0)
view.mrmlViewNode().SetBackgroundColor2(0,0,0)
view.forceRender()

# Get the path of the currently loaded scene
scenePath = slicer.mrmlScene.GetURL()

# Extract the file name from the full path
import os
fileName = os.path.basename(scenePath)

print(f"Current path: {scenePath}")
print(f"Current file name: {fileName}")

# ectract CV number from scenePath
import re

# Regular expression to match 'pattern' followed by numbers
pattern = r"(CV\d+)"

# Find all matches
match  = re.findall(pattern, scenePath)

if match :
    curr_CV = match[0]
    
# print(curr_CV)

# Capture RGBA image
curr_image_file_name = str(curr_CV) + ".png"
print(f"Current image name: {curr_image_file_name}")

renderWindow = view.renderWindow()
renderWindow.SetAlphaBitPlanes(1)
wti = vtk.vtkWindowToImageFilter()
wti.SetInputBufferTypeToRGBA()
wti.SetInput(renderWindow)
writer = vtk.vtkPNGWriter()
writer.SetFileName(curr_CV + ".png")
writer.SetInputConnection(wti.GetOutputPort())
writer.Write()

# resotre bounding box and axis labels
viewNode.SetBoxVisible(True)
viewNode.SetAxisLabelsVisible(True)

# restore visibility of markers
for node in fiducialNodes:
    node.SetDisplayVisibility(True)
