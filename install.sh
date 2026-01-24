#!/bin/bash
# ParenVault Installation Script
# Run this on a new machine to set up ParenVault

set -e

echo "=== ParenVault Installation ==="
echo ""

# Check for OCaml/opam
if ! command -v opam &> /dev/null; then
    echo "❌ opam not found. Please install OCaml and opam first:"
    echo "   https://ocaml.org/docs/installing-ocaml"
    exit 1
fi

echo "✓ opam found"

# Check opam is initialized
if [ ! -d "$HOME/.opam" ]; then
    echo "Initializing opam..."
    opam init -y
fi

# Ensure we're using the right switch
eval $(opam env)

echo ""
echo "=== Installing OCaml dependencies ==="
opam install -y \
    dune \
    caqti \
    caqti-lwt \
    caqti-driver-sqlite3 \
    caqti-driver-postgresql \
    lwt \
    lwt_ppx \
    notty \
    lwd \
    ptime \
    uuidm \
    yojson \
    toml \
    ppx_deriving

echo ""
echo "=== Building ParenVault ==="
dune build

echo ""
echo "=== Creating directories ==="
mkdir -p ~/.config/parenvault
mkdir -p ~/.local/share/parenvault
mkdir -p ~/.local/share/parenvault/attachments
mkdir -p ~/parenvault-export

echo ""
echo "=== Creating config file ==="
if [ ! -f ~/.config/parenvault/config.toml ]; then
    HOSTNAME=$(hostname)
    cat > ~/.config/parenvault/config.toml << EOF
[device]
name = "$HOSTNAME"

[database]
local_path = "$HOME/.local/share/parenvault/parenvault.db"
remote_host = "your-server.tailnet.ts.net"
remote_port = 5432
remote_name = "parenvault"
remote_user = "parenvault"
remote_password = ""

[sync]
auto_sync = true
interval_secs = 300
EOF
    echo "✓ Created config at ~/.config/parenvault/config.toml"
    echo "  ⚠️  Edit this file to set your PostgreSQL remote server details"
else
    echo "✓ Config already exists"
fi

echo ""
echo "=== Creating launcher script ==="
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cat > ~/.local/bin/pv << EOF
#!/bin/bash
cd "$SCRIPT_DIR"
dune exec parenvault -- tui 2>/dev/null
EOF
chmod +x ~/.local/bin/pv

echo ""
echo "=== Installation Complete ==="
echo ""
echo "To run ParenVault:"
echo "  pv"
echo ""
echo "Or from this directory:"
echo "  ./pv"
echo ""
echo "Make sure ~/.local/bin is in your PATH:"
echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
echo ""
