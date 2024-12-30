FROM haskell:9.6.6-slim as builder

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
RUN stack install --local-bin-path .

FROM debian:12.8
RUN apt-get update && apt-get install -y libpq-dev
COPY --from=builder /opt/example/Clouds-Lab3-exe /usr/local/bin/Clouds-Lab3-exe

# Set the working directory
WORKDIR /opt/example

EXPOSE 8000

CMD ["Clouds-Lab3-exe"]