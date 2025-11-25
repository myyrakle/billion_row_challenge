#!/bin/sh
docker run --rm \
  -v "$PWD:/app" \
  -w /app \
  rust:1.82 \
  cargo run --release --manifest-path ./handler/Cargo.toml
