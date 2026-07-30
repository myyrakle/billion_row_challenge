rustup toolchain list 2>/dev/null | grep -q '^nightly' || { echo "[myyrakle1] installing rust nightly toolchain (first run only)..."; rustup toolchain install nightly; }
RUSTFLAGS="-C target-cpu=native" cargo +nightly run --release --manifest-path ./rust/myyrakle1/Cargo.toml
