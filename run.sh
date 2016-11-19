#!/bin/sh
elm-make src/Monad.elm --yes --output build/app.js && node src/main.js $@
