#!/bin/bash -xe

readonly APFS_UTIL=/System/Library/Filesystems/apfs.fs/Contents/Resources/apfs.util

# 0. Config
readonly WARP_MOUNT_POINT=/warp_root
readonly WARP_VOLUME_LABEL="warp Store"

# 1. Find the right Disk
readonly ROOT_DISK_IDENTIFIER=$(
  /usr/sbin/diskutil info -plist / \
    | xmllint --xpath "/plist/dict/key[text()='ParentWholeDisk']/following-sibling::string[1]/text()" -
)

# 2. Create a new volume
readonly WARP_VOLUME_NAME=$(
  /usr/sbin/diskutil apfs \
    addVolume "$ROOT_DISK_IDENTIFIER" "apfs" "$WARP_VOLUME_LABEL" \
    -nomount \
    | /usr/bin/awk '/Created new APFS Volume/ {print $5}'
)

# 3. Get the new volume UUID
readonly WARP_VOLUME_ID=$( ${APFS_UTIL} -k "${WARP_VOLUME_NAME}" )

# 4. Create a Daemon that will mount the volume onto /warp
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

launchctl bootstrap system "$WARP_VOLUME_MOUNTD_DEST" || true
launchctl kickstart -k system/dev.abstractmachines.warp.store

# 5. Create the mount point on /warp  (fstab)
echo -e "UUID=${WARP_VOLUME_ID} ${WARP_MOUNT_POINT} apfs rw,noauto,nobrowse,suid,owners" | sudo tee -a /etc/fstab >/dev/null

# 6. Mount the disk
diskutil mount ${WARP_VOLUME_ID}

# 7. Create the synthetic root level /warp pointer
echo -e "${WARP_MOUNT_POINT}" | sed 's,/,,g' | sudo tee -a /etc/synthetic.conf >/dev/null
{ $APFS_UTIL -B || true; $APFS_UTIL -t || true; } >/dev/null 2>&1
