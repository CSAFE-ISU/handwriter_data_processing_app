#========================================================
#============= ADDING TOOL TIPS TO UI ===================
#========================================================

#PRE PROCESSING 
addTooltip(session, id = 'rotation', title = "Slide to rotate document. Use arrows to adjust by 1 degree", options = list(delay = list(show=500)))
addTooltip(session, id = 'reset_crop', title = "Reset to originally uploaded image.", options = list(delay = list(show=500)))
addTooltip(session, id = 'undo_crop', title = "Undo previous crop", options = list(delay = list(show=500)))
addTooltip(session, id = 'crop', title = "Crop the highlighted area", options = list(delay = list(show=500)))
addTooltip(session, id = 'mask', title = "Mask the highlighted area", options = list(delay = list(show=500)))
addTooltip(session, id = 'undo_mask', title = "Undo previously applied mask", options = list(delay = list(show=500)))
addTooltip(session, id = 'reset_mask', title = "Undo all mask areas", options = list(delay = list(show=500)))
addTooltip(session, id = 'save_mask', title = "Save masks as an RData Object. The object will have the document and the mask, and must be saved with the .RData file type", options = list(delay = list(show=500)))
