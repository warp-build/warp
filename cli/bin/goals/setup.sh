#!/bin/bash -xe


readonly CURRENT_USER=$(whoami)

echo "Beginning setup for ${CURRENT_USER}..."

readonly APFS_UTIL=/System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util

# 0. Config
readonly WARP_MOUNT_POINT=/warp
readonly WARP_VOLUME_LABEL="warp Store"

# 1. Find the right Disk
readonly ROOT_DISK_IDENTIFIER=$(
  /usr/sbin/diskutil info -plist / \
    | xmllint --xpath "/plist/dict/key[text()='ParentWholeDisk']/following-sibling::string[1]/text()" -
)
echo "Found main disk at: ${ROOT_DISK_IDENTIFIER}"
echo "Creating the Warp volume now..."

# 2. Create a new volume
readonly WARP_VOLUME_NAME=$(
  /usr/sbin/diskutil apfs \
    addVolume "$ROOT_DISK_IDENTIFIER" "apfs" "$WARP_VOLUME_LABEL" \
    -nomount \
    | /usr/bin/awk '/Created new APFS Volume/ {print $5}'
)

# 3. Get the new volume UUID
readonly WARP_VOLUME_ID=$( ${APFS_UTIL} -k "${WARP_VOLUME_NAME}" )
echo "Warp volume created with id: ${WARP_VOLUME_ID}"

# 4. Create a Daemon that will mount the volume onto /warp
echo "Setting up the Warp daemon..."
readonly WARP_VOLUME_MOUNTD_DEST=/Library/LaunchDaemons/dev.abstractmachines.warp.store.plist
sudo tee ${WARP_VOLUME_MOUNTD_DEST} >/dev/null <<EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple Computer//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
  <key>RunAtLoad</key>
  <true/>
  <key>Label</key>
  <string>dev.abstractmachines.warp.store</string>
  <key>ProgramArguments</key>
  <array>
    <string>/usr/sbin/diskutil</string>
    <string>mount</string>
    <string>-mountPoint</string>
    <string>$WARP_MOUNT_POINT</string>
    <string>$WARP_VOLUME_ID</string>
  </array>
</dict>
</plist>
EOF

sudo launchctl bootstrap system "$WARP_VOLUME_MOUNTD_DEST" || true
sudo launchctl kickstart -k system/dev.abstractmachines.warp.store
echo "Warp daemon set up!"

# 5. Create the mount point on /warp  (fstab)
echo "Mounting Warp volume onto ${WARP_MOUNT_POINT}..."
echo -e "UUID=${WARP_VOLUME_ID} ${WARP_MOUNT_POINT} apfs rw,noauto,nobrowse,suid,owners" | sudo tee -a /etc/fstab >/dev/null

# 6. Create the synthetic root level /warp pointer
echo -e "${WARP_MOUNT_POINT}" | sed 's,/,,g' | sudo tee -a /etc/synthetic.conf >/dev/null
{ $APFS_UTIL -B || true; $APFS_UTIL -t || true; } >/dev/null 2>&1

# 7. Mount the disk
diskutil mount ${WARP_VOLUME_ID}
echo "Warp volume mounted!"

# 8. Give write permissions to /warp folder
echo "Adjusting permissions..."
sudo chown -R ${CURRENT_USER}: ${WARP_MOUNT_POINT}
echo "Permissions set!"
echo ""
echo "This machine is Warp-capable now."
