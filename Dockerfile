FROM haskell:9.6.6-slim

WORKDIR /opt/example

RUN apt-get update && apt-get install -y libpq-dev
# RUN cabal update

# Add just the .cabal file to capture dependencies
COPY package.yaml stack.yaml stack.yaml.lock /opt/example/


# Docker will cache this command as a layer, freeing us up to
# modify source code without re-installing dependencies
# (unless the .cabal file changes!)
# RUN cabal build --only-dependencies -j4
RUN stack build --only-dependencies



# Add and Install Application Code
COPY . /opt/example
RUN stack install
EXPOSE 8000

CMD ["stack", "run"]