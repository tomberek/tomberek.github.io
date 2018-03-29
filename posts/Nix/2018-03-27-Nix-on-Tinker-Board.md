---
title: NixOS on Tinker Board
author: tomberek
toc: yes
tags: Nix NixOS
---

## Introduction
Trying to get [NixOS on a RaspberryPi][NixOS_on_ARM] the initial build and lack of a reliable binary cache suggets the idea to produce our own for ARMv7l. A [Tinker Board][] is a faster version and this post explains how to port NixOS to it.

## Initial Attempts
The [NixOS_on_ARM][] documentation suggests simply trying `sd-image-armv7l-linux.img`, but this does not work. The TinkerOS image (i used `20170417-tinker-board-linaro-strech-alip-v1.8.img`) worked well. So the next step was to investigate differences and obtain some debugging access.

[NixOS_on_ARM]: https://nixos.wiki/wiki/NixOS_on_ARM
[Tinker Board]: https://www.asus.com/us/Single-Board-Computer/Tinker-Board/

### Console Access
Using a SparkFun ProMicro (5V) and the SerialPassthrough example sketch, connecting via pins 8 and 10, we can obtain a console output of U-Boot of TinkerOS. It shows an invalid CRC, provides a default environment, and boots the OS. It turns out that getting into U-Boot using the TinkerOS image, halting the boot process, replacing SD cards with the NixOS image fails, but the error message suggests the extlinux merely has a different name for the DTB than NixOS has packaged. Simply copy from the nixos dtb file `/boot/nixos/<hash>-dtbs/rk3288-tinker.dtb` to `rockchip-tinker_rk3288.dtb` (possibly `rockchip-miniarm_rk3288.dtb`, TODO: examine U-Boot output to see what DTB it tries to read).

With the proper DTB, NixOS should boot with HDMI output, but nothing to console. This is due to the console setup. There seems to be contention for serial ports in the default `boot.kernelParams` so we add:

```nix
boot.kernelParams = [
    "console=ttymxc0,115200n8" # HDMI?
    "console=tty0"             # Primary Console?
    "console=ttyS1,115200n8"   # Serial port?
    "console=ttyS2,115200n8"   # Serial port pins 8+10
 ];
```
to `configuration.nix`. With this there should be HDMI as well as serial access to make changes. Moving on to U-boot...

## U-Boot

The Tinker Board seems to look for U-Boot at a specific offset rather than in a file. So copy the initial 4MB from the TinkerOS image and then copy it to the NixOS image while skipping the MBR/partition table in the first 512 bytes. It turns out the bootstrap section of the MBR is also needed, so copy that as well.

These commands may use either the /dev/sdX or the file images and later copied to SD cards - this was discovered iteratively, thus was performed on the sd cards directly. Use with caution.
```sh
dd if=/dev/sdc of=tinker_sector bs=512k count=8 status=progress # grab tinker initial sectors
dd if=tinker_sector of=/dev/sdb bs=512 skip=1 seek=1 count=8191 # copy the img and env blobs
dd of=/dev/sdb if=tinker_sector bs=428 count=1 # bootstrap code from begining of MBR
```

Now U-boot should start with the same version/text as the TinkerOS image, then boot into NixOS using the renamed DTB from the NixOS image, providing both a HDMI and serial console.

## NixOS configuration
This is mostly what @dezgeg recommends. Primary changes are resolving redirects to the binary cache. (Note: the earthtools cache doesn't seems to work at the moment.) There is a problem with certain packages at @dezgeg's cache that are not signed with the key the wiki post recommends, so those got moved to `nix.trustedBinaryCaches`. (TODO: fix this? contact @dezgeg?)
```nix
{ config, pkgs, lib, ... }:
{
  # NixOS wants to enable GRUB by default
  boot.loader.grub.enable = false;
  # Enables the generation of /boot/extlinux/extlinux.conf
  boot.loader.generic-extlinux-compatible.enable = true;
 
  boot.kernelPackages = pkgs.linuxPackages_latest;
  
  nix.binaryCaches = lib.mkForce [ "https://cache.nixos.org" ];
  nix.trustedBinaryCaches = lib.mkForce [
	                "https://www.cs.helsinki.fi/u/tmtynkky/nixos-arm/channel/" ];
  #nix.binaryCachePublicKeys = [ "nixos-arm.dezgeg.me-1:xBaUKS3n17BZPKeyxL4JfbTqECsT+ysbDJz29kLFRW0=%" ];
  #nix.requireSignedBinaryCaches = false;

  nix.buildCores = 4;
  nix.maxJobs = 4;
  nix.extraOptions = ''
    gc-keep-outputs = true
    gc-keep-derivations = true
  '';

  # !!! Needed for consoles to work on Tinker Board
  boot.kernelParams = [
    "console=ttymxc0,115200n8"
    "console=tty0"
    "console=ttyS1,115200n8"
    "console=ttyS2,115200n8"
 ];
 boot.consoleLogLevel = 7;
    
  # File systems configuration for using the installer's partition layout
  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };
  environment.systemPackages = with pkgs; [ file vim gitAndTools.git ];

  # Careful, security issues ahead!
  services.openssh = {
    enable = true;
    permitRootLogin= "yes"; # careful
  };
  users.users.root.initialHashedPassword = "";
}
```

## Next steps

- [ ] Reproduce a working U-Boot from upstream
- [ ] Create Nix expression to build SD image
- [ ] Host binary cache (either via transfer to x86 server or native)
