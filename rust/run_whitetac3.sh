rustup toolchain list 2>/dev/null | grep -q '^nightly' || { echo "[whitetac3] installing rust nightly toolchain (first run only)..."; rustup toolchain install nightly; }
RUSTFLAGS="-C target-cpu=native" cargo +nightly run --release --manifest-path ./rust/whitetac3/Cargo.toml
