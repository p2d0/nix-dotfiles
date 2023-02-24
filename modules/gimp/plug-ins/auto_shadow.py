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

def python_auto_shadow(image,layer, thickness = 5):
    gimp_log("pepegas")
    pdb.gimp_image_select_item(image, 0, layer)
    print("pepegas")
    pdb.gimp_selection_grow(image, thickness)
    sublayer = pdb.gimp_layer_new(image,image.width,image.height,RGBA_IMAGE,layer.name + "_outline",100, NORMAL_MODE)
    pdb.gimp_image_insert_layer(image, sublayer, None, 1+pdb.gimp_image_get_item_position(image, layer))
    # pdb.gimp_context_swap_color()
    pdb.gimp_edit_fill(sublayer, 1)
    pdb.gimp_selection_none(image)
    group = pdb.gimp_layer_group_new(image)
    pdb.gimp_image_insert_layer(image,group,None,0)
    move_layer_to_group(image,sublayer,group)
    move_layer_to_group(image,layer,group)
    pdb.gimp_selection_none(image)


register(
    "python_fu_auto_shadow",
    "Create a 3-D effect",
    "Create a blobby 3-D effect using inverse drop-shadow",
    "Akkana Peck",
    "Akkana Peck",
    "2009",
    "AutoShadow",
    "*",
    [
        (PF_IMAGE, "image", "Input image", None),
        (PF_DRAWABLE, "drawable", "Input drawable", None),
        (PF_INT, "thickness", "Text thickness", 5),
    ],
    [],
    python_auto_shadow,
    menu="<Image>/Filters")


main()
