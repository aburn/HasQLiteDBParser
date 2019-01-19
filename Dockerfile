# Use the official haskell image... for now this pjt seems to use 8.4.4, lets continue to use it.
FROM haskell:8.4.4

# Checkout code into docker container
WORKDIR /parser
ADD . /parser

# ideally we want to build and run few tests before starting..
# we are just beggining, just build run and exit for now
RUN stack setup
RUN stack build

# For now no need to expose any ports but lets see what happens.
EXPOSE 80

CMD ["stack", "exec", "sqlitedbparser-exe", "--", "test/places.sqlite"]
