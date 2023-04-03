#!/bin/bash -xe

readonly CURRENT_USER=$(whoami)

echo "Beginning setup for ${CURRENT_USER}..."

# 0. Config
readonly WARP_ROOT=/warp

# 1. Create warp root
sudo mkdir ${WARP_ROOT}

# 2. Give write permissions to /warp folder
echo "Adjusting permissions..."
sudo chown -R ${CURRENT_USER}: ${WARP_ROOT}
echo "Permissions set!"
echo ""
echo "This machine is Warp-capable now."
