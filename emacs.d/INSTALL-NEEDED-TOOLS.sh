# You'll need to manually install some tools to get everything working

# Install some system packages
sudo dnf update --refresh -y 
sudo dnf install -y clang bear ripgrep fd-find clisp

# Install rustup and rust
if ! command -v rustup &> /dev/null; then
    curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
fi

# Update to the latest rust version
rustup update

# Add rust source code, needed for rust-analyzer
rustup component add rust-src

# Build rust-analyzer and install it
git clone https://github.com/rust-analyzer/rust-analyzer.git
cd rust-analyzer
cargo xtask install --server

# Install python language server
pip3 install --user wheel pyls python-language-server[all]
