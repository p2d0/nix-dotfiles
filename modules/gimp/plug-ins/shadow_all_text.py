#!/usr/bin/env python2
from gimpfu import *
# import sys
# sys.stderr = open('~/er.txt', 'a')
# sys.stdout = open('~/gogo.txt', 'a')

def gimp_log(text):
    pdb.gimp_message(text)

def move_layer_to_group(image,layer,group):
    orig_name = layer.name
    ly2 = pdb.gimp_layer_copy(layer,True)
    pdb.gimp_image_insert_layer(image, ly2, group, 0)
    pdb.gimp_image_remove_layer(image,layer)
    ly2.name = orig_name

def _rec(layers, lst):
    for l in lst:
        if not l.visible: continue

        if pdb.gimp_item_is_group(l): _rec(layers,l.layers)
        else: layers.append(l)

    return layers

def _get_layers(image, dont_recurse=False):
    layers = None

    if dont_recurse:
        layers = [layer for layer in image.layers if (
            layer.visible and (not pdb.gimp_item_is_group(layer)) and pdb.gimp_drawable_is_text_layer(layer))]
    else:
        layers = _rec([], image.layers)
    return layers

def python_auto_shadow(image,layer, thickness = 5):
    pdb.gimp_image_select_item(image, 0, layer)
    pdb.gimp_selection_grow(image, thickness)
    sublayer = pdb.gimp_layer_new(image,layer.width+(thickness*2),layer.height+(thickness*2),RGBA_IMAGE,layer.name + "_with_outline",100, NORMAL_MODE)
    x_off, y_off = layer.offsets
    pdb.gimp_layer_translate(sublayer,x_off-thickness,y_off-thickness)
    pdb.gimp_image_insert_layer(image, sublayer, None, 1+pdb.gimp_image_get_item_position(image, layer))
    pdb.gimp_edit_fill(sublayer, 1)
    pdb.gimp_image_merge_down(image,layer,0)
    pdb.gimp_selection_none(image)

def python_shadow_all_text(image, thickness = 5):
    gimp.progress_init("Adding shadows")
    pdb.gimp_undo_push_group_start(image)
    layers = _get_layers(image,False)
    for layer in layers:
        if pdb.gimp_drawable_is_text_layer(layer):
            python_auto_shadow(image,layer,thickness)
    pdb.gimp_progress_end()
    pdb.gimp_undo_push_group_end(image)


register(
    "python_fu_shadow_all_text",
    "Shadow all text",
    "Shadow all text",
    "Andrew Cerkin",
    "Andrew Cerkin",
    "2022",
    "Shadow_AllText",
    "*",
    [
        (PF_IMAGE, "image", "Input image", None),
        (PF_INT, "thickness", "Text thickness", 5),
    ],
    [],
    python_shadow_all_text,
    menu="<Image>/Filters/Generic")


main()
