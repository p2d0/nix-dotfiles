#!/usr/bin/env python3
import os
import mammoth
from mammoth import cli
import shutil


def get_out_dir(path):
    return os.path.splitext(path)[0]



def extract_mammoth(path):
    out_dir = get_out_dir(path);
    convert_image = mammoth.images.inline(cli.ImageWriter(out_dir))
    os.mkdir(out_dir)
    result = mammoth.convert_to_html(path,convert_image=convert_image)


if __name__ == '__main__':
    pathsEnv = os.environ["NAUTILUS_SCRIPT_SELECTED_FILE_PATHS"];
    paths = pathsEnv.split("\n")
    for path in paths:
        if(path != ''):
            extract_mammoth(path)
