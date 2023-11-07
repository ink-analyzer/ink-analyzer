# Base image.
FROM rust:1-slim-bullseye

# Create app working directory.
WORKDIR /app

# Copy source code.
COPY . .

# Set `cargo test` as the executable.
ENTRYPOINT ["/usr/local/cargo/bin/cargo", "test"]
