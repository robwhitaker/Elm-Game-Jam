# Elm Game Jam 2018

My entry for the itch.io Elm Game Jam.

### Dependencies

- Elm
- ImageMagick
- Spriter

### Build Notes

Project can be built by running `elm-make src/Main.elm`.

If sprites are updated in Spriter, export the strips into the `sprite-projects/render/SPRITESHEET_NAME` folder, then run `./buildSpritesheets.sh` to concatenate the strips into spritesheets and make the output a power of two size (necessary for rendering here).