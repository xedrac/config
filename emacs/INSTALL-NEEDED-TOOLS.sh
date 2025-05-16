# You'll need to manually install some tools to get everything working

# Install some system packages
sudo dnf update --refresh -y 
sudo dnf install -y clang-devel bear ripgrep fd-find locate libvterm cmake

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

# Install Common Lisp stuff
#sudo dnf install sbcl
#mkdir -p ~/quicklisp
#pushd ~/quicklisp
#curl -O https://beta.quicklisp.org/quicklisp.lisp
#popd

#echo ""
#echo ""
#echo "-------------------------------------------------------"
#echo "Inside the sbcl repl, run the following expressions:"
#echo ""
#echo "    (quicklisp-quickstart:install)"
#echo "    (ql:add-to-init-file)"
#echo "    (ql:quickload \"quicklisp-slime-helper\")"
#echo ""
#echo "-------------------------------------------------------"
#echo ""
#echo ""
#
#sbcl --load ~/quicklisp/quicklisp.lisp

#sbcl --load ~/quicklisp/quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
#sbcl --load ~/quicklisp/setup.lisp --eval "(ql:add-to-init-file)" --quit
