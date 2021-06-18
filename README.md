# Dumb mcclim statusbar
A starting place for an X11 status bar implemented with mcclim's CLX backend.

For the moment the content is mostly placeholders, but I've written enough logic
that I can easily add more display items and the bar will (more or less)
properly lay everything out for me.

## Running it
A clpm bundle and lockfile is included if you want to get the dependencies
exactly right. The entrypoint is site.djei.bar:main. Right now it probably will
not work properly on anything but a 1920x1080 single monitor setup.

## A screenshot
![An image](screenshot.png)
