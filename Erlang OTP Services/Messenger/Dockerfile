FROM erlang:26-alpine AS build
WORKDIR /src
RUN apk add --no-cache git
COPY . .

# Build the production release
RUN rebar3 as prod release

FROM alpine:3.19
# libgcc and libstdc++ are often needed for NIFs or specific Erlang drivers
RUN apk add --no-cache openssl ncurses-libs libstdc++ libgcc

WORKDIR /app

# Copy the release from the prod profile
COPY --from=build /src/_build/prod/rel/Messenger ./

# Set these environment variables for better runtime behavior
ENV RELX_REPLACE_OS_VARS=true
ENV ERL_EPMD_PORT=4369

# Expose App Port, EPMD, and Distribution Range
EXPOSE 5555 4369 9100-9105

ENTRYPOINT ["/app/bin/Messenger"]
CMD ["foreground"]