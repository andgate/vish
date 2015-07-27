# vish
Visual Novel Engine written with Haskell.

# File layout
Actors and their corresponding expressions are loaded when a script that depends on them is run.
The script searches the data/actor directory for a folder with the actor's name, then searches
that folder for a file named by the corresponding expression, and tries to load it. If there are
multiple files with that name, or no files, and error is thrown. Any unsupported image format will
also throw an error. Supported image formats are "bmp", "jpg", "png", "tga", and "tiff".
For instance, say the script uses an actor named Jotaro who has a cool expression. The image file
for Jotaro and his cool expression could be "data/actor/Jotaro/cool.png".
