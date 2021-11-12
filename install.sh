#!/bin/sh
set -x
set -e

if [ ! "$USER" = "root" ]; then
  echo "Must be run as root!"
  exit 1
fi

read -p "What disk to format (e.g. /dev/sda, /dev/nvme0n1)? " DISK
ls $DISK*
#read -p "What hostname (usually a brief description of the device)? " HOSTNAME

INST_UUID=$(dd if=/dev/urandom bs=1 count=100 2>/dev/null | tr -dc 'a-z0-9' | cut -c-6)
INST_ID=nixos
INST_CONFIG_FILE='zfs.nix'

INST_PRIMARY_DISK=$(echo $DISK | cut -f1 -d\ )

# not set for single disk or striped
INST_VDEV=
INST_PARTSIZE_ESP=2 # in GB
INST_PARTSIZE_BPOOL=4
INST_PARTSIZE_SWAP=16

# root pool size, use all remaining disk if not set
INST_PARTSIZE_RPOOL=

# wipe solid-state drives with the generic tool blkdiscard, to clean previous partition tables and improve performance
for i in ${DISK}; do
  blkdiscard -f $i &
done
wait

for i in ${DISK}; do
  sgdisk --zap-all $i
  sgdisk -n1:1M:+${INST_PARTSIZE_ESP}G -t1:EF00 $i
  sgdisk -n2:0:+${INST_PARTSIZE_BPOOL}G -t2:BE00 $i
  if [ "${INST_PARTSIZE_SWAP}" != "" ]; then
    sgdisk -n4:0:+${INST_PARTSIZE_SWAP}G -t4:8200 $i
  fi
  if [ "${INST_PARTSIZE_RPOOL}" = "" ]; then
    sgdisk -n3:0:0   -t3:BF00 $i
  else
    sgdisk -n3:0:+${INST_PARTSIZE_RPOOL}G -t3:BF00 $i
  fi
  sgdisk -a1 -n5:24K:+1000K -t5:EF02 $i
done


# create boot pool
disk_num=0; for i in $DISK; do disk_num=$(( $disk_num + 1 )); done
if [ $disk_num -gt 1 ]; then INST_VDEV_BPOOL=mirror; fi


zpool create \
-d -o feature@async_destroy=enabled \
-o feature@bookmarks=enabled \
-o feature@embedded_data=enabled \
-o feature@empty_bpobj=enabled \
-o feature@enabled_txg=enabled \
-o feature@extensible_dataset=enabled \
-o feature@filesystem_limits=enabled \
-o feature@hole_birth=enabled \
-o feature@large_blocks=enabled \
-o feature@lz4_compress=enabled \
-o feature@spacemap_histogram=enabled \
    -o ashift=12 \
    -o autotrim=on \
    -O acltype=posixacl \
    -O canmount=off \
    -O compression=lz4 \
    -O devices=off \
    -O normalization=formD \
    -O relatime=on \
    -O xattr=sa \
    -O mountpoint=/boot \
    -R /mnt \
    bpool_$INST_UUID \
     $INST_VDEV_BPOOL \
    $(for i in ${DISK}; do
       printf "${i}2 ";
      done)

# creat root pool
zpool create \
    -o ashift=12 \
    -o autotrim=on \
    -R /mnt \
    -O acltype=posixacl \
    -O canmount=off \
    -O compression=zstd \
    -O dnodesize=auto \
    -O normalization=formD \
    -O relatime=on \
    -O xattr=sa \
    -O mountpoint=/ \
    rpool_$INST_UUID \
    $INST_VDEV \
   $(for i in ${DISK}; do
      printf "${i}3 ";
     done)

# create encrypted root container
zfs create \
 -o canmount=off \
 -o mountpoint=none \
 -o encryption=aes-256-gcm \
 -o keylocation=prompt \
 -o keyformat=passphrase \
 rpool_$INST_UUID/$INST_ID

# Create other system datasets
zfs create -o canmount=off -o mountpoint=none bpool_$INST_UUID/$INST_ID
zfs create -o canmount=off -o mountpoint=none bpool_$INST_UUID/$INST_ID/BOOT
zfs create -o canmount=off -o mountpoint=none rpool_$INST_UUID/$INST_ID/ROOT
zfs create -o canmount=off -o mountpoint=none rpool_$INST_UUID/$INST_ID/DATA
zfs create -o mountpoint=/boot -o canmount=noauto bpool_$INST_UUID/$INST_ID/BOOT/default
zfs create -o mountpoint=/ -o canmount=off    rpool_$INST_UUID/$INST_ID/DATA/default
zfs create -o mountpoint=/ -o canmount=off    rpool_$INST_UUID/$INST_ID/DATA/local
zfs create -o mountpoint=/ -o canmount=noauto rpool_$INST_UUID/$INST_ID/ROOT/default
zfs mount rpool_$INST_UUID/$INST_ID/ROOT/default
zfs mount bpool_$INST_UUID/$INST_ID/BOOT/default
for i in {usr,var,var/lib};
do
    zfs create -o canmount=off rpool_$INST_UUID/$INST_ID/DATA/default/$i
done
for i in {home,root,srv,usr/local,var/log,var/spool};
do
    zfs create -o canmount=on rpool_$INST_UUID/$INST_ID/DATA/default/$i
done
chmod 750 /mnt/root
for i in {nix,}; do
    zfs create -o canmount=on -o mountpoint=/$i rpool_$INST_UUID/$INST_ID/DATA/local/$i
done


# Datasets for immutable root filesystem
zfs create -o canmount=on rpool_$INST_UUID/$INST_ID/DATA/default/state
for i in {/etc/nixos,/etc/cryptkey.d}; do
  mkdir -p /mnt/state/$i /mnt/$i
  mount -o bind /mnt/state/$i /mnt/$i
done
zfs create -o mountpoint=/ -o canmount=noauto rpool_$INST_UUID/$INST_ID/ROOT/empty
zfs snapshot rpool_$INST_UUID/$INST_ID/ROOT/empty@start

# Format and mount ESP
for i in ${DISK}; do
  mkfs.vfat -n EFI ${i}1
  mkdir -p /mnt/boot/efis/${i##*/}1
  mount -t vfat ${i}1 /mnt/boot/efis/${i##*/}1
done

nixos-generate-config --root /mnt

# Edit config file to import ZFS options
sed -i "s|./hardware-configuration.nix|./hardware-configuration-zfs.nix ./${INST_CONFIG_FILE}|g" /mnt/etc/nixos/configuration.nix
# backup, prevent being overwritten by nixos-generate-config
mv /mnt/etc/nixos/hardware-configuration.nix /mnt/etc/nixos/hardware-configuration-zfs.nix

# ZFS options
tee -a /mnt/etc/nixos/${INST_CONFIG_FILE} <<EOF
{ config, pkgs, ... }:

{ boot.supportedFilesystems = [ "zfs" ];
  networking.hostId = "$(head -c 8 /etc/machine-id)";
  boot.zfs.devNodes = "${INST_PRIMARY_DISK%/*}";
EOF


# ZFS datasets should be mounted with -o zfsutil option:
sed -i 's|fsType = "zfs";|fsType = "zfs"; options = [ "zfsutil" ];|g' \
/mnt/etc/nixos/hardware-configuration-zfs.nix

# Allow EFI system partition mounting to fail at boot:
sed -i 's|fsType = "vfat";|fsType = "vfat"; options = [ "x-systemd.idle-timeout=1min" "x-systemd.automount" "noauto" ];|g' \
/mnt/etc/nixos/hardware-configuration-zfs.nix

# Disable cache:
mkdir -p /mnt/state/etc/zfs/
rm -f /mnt/state/etc/zfs/zpool.cache
touch /mnt/state/etc/zfs/zpool.cache
chmod a-w /mnt/state/etc/zfs/zpool.cache
chattr +i /mnt/state/etc/zfs/zpool.cache

# If swap is enabled:
if [ "${INST_PARTSIZE_SWAP}" != "" ]; then
    sed -i '/swapDevices/d' /mnt/etc/nixos/hardware-configuration-zfs.nix

    tee -a /mnt/etc/nixos/${INST_CONFIG_FILE} <<EOF
      swapDevices = [
    EOF
  for i in $DISK; do
    tee -a /mnt/etc/nixos/${INST_CONFIG_FILE} <<EOF
      { device = "${i}4"; randomEncryption.enable = true; }
    EOF
  done
  tee -a /mnt/etc/nixos/${INST_CONFIG_FILE} <<EOF
    ];
  EOF
fi

# Configure GRUB boot loader for both legacy boot and UEFI:

sed -i '/boot.loader/d' /mnt/etc/nixos/configuration.nix
tee -a /mnt/etc/nixos/${INST_CONFIG_FILE} <<EOF
  boot.loader = {
    generationsDir.copyKernels = true;
    ##for problematic UEFI firmware
    grub.efiInstallAsRemovable = true;
    efi.canTouchEfiVariables = false;
    ##if UEFI firmware can detect entries
    #efi.canTouchEfiVariables = true;
    efi.efiSysMountPoint = "/boot/efis/${INST_PRIMARY_DISK##*/}1";
    grub.enable = true;
    grub.version = 2;
    grub.copyKernels = true;
    grub.efiSupport = true;
    grub.zfsSupport = true;
    # for systemd-autofs
    grub.extraPrepareConfig = ''
      mkdir -p /boot/efis
      for i in  /boot/efis/*; do mount \$i ; done
    '';
    grub.devices = [
EOF
for i in $DISK; do
  printf "      \"$i\"\n" >>/mnt/etc/nixos/${INST_CONFIG_FILE}
done
tee -a /mnt/etc/nixos/${INST_CONFIG_FILE} <<EOF
    ];
    grub.mirroredBoots = [
EOF
for i in $DISK; do
  printf "      { devices = [ \"$i\" ] ; efiSysMountPoint = \"/boot/efis/${i##*/}1\"; path = \"/boot\"; }\n" \
  >>/mnt/etc/nixos/${INST_CONFIG_FILE}
done
tee -a /mnt/etc/nixos/${INST_CONFIG_FILE} <<EOF
    ];
  };
EOF
