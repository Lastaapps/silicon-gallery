# This file is "complex" just to support dependencies caching,
# which I want on my RPi
# `stack` cannot be used as it does not support ARM64
FROM haskell:9.8.4-slim

WORKDIR /opt/silicon-gallery

RUN cabal update

# Add just the .cabal file to capture dependencies
COPY ./silicon-gallery.cabal /opt/silicon-gallery/silicon-gallery.cabal

# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
RUN cabal build --only-dependencies -j4

# Add and Install Application Code
COPY . /opt/silicon-gallery
RUN cabal install

ENTRYPOINT ["silicon-gallery"]

