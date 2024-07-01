from gimpfu import *

def batch_crop(image, layer, start_pos, num_crops, direction, output_folder):
    crop_size = 512

    for i in range(num_crops):
        if direction == "x":
            x = start_pos + (i * crop_size)
            y = 0
        else:
            x = 0
            y = start_pos + (i * crop_size)

        # Duplicate the image
        duplicated_image = pdb.gimp_image_duplicate(image)
        duplicated_layer = pdb.gimp_image_get_active_layer(duplicated_image)

        # Crop the duplicated image to the specific region
        pdb.gimp_image_crop(duplicated_image, crop_size, crop_size, x, y)

        # Merge visible layers to ensure a single layer for exporting
        merged_layer = pdb.gimp_image_merge_visible_layers(duplicated_image, CLIP_TO_IMAGE)

        # Save the cropped image
        output_path = "{}/Crop-{}.png".format(output_folder, i + 1)
        pdb.file_png_save(duplicated_image, merged_layer, output_path, output_path, 0, 9, 1, 1, 1, 1, 1)

        # Clean up
        pdb.gimp_image_delete(duplicated_image)

register(
    "python_fu_batch_crop",
    "Batch crop an image",
    "Batch crop an image at specified intervals",
    "Your Name",
    "Your Name",
    "2024",
    "<Image>/Filters/Batch Crop",
    "*",
    [
        (PF_INT, "start_pos", "Starting Position", 0),
        (PF_INT, "num_crops", "Number of Crops", 40),
        (PF_RADIO, "direction", "Direction", "y", (("X", "x"), ("Y", "y"))),
        (PF_DIRNAME, "output_folder", "Output Folder", "/home/andrew/output")
    ],
    [],
    batch_crop)

main()
